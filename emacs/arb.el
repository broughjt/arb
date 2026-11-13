;;; -*- lexical-binding: t; -*-

(require 'calc)
(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'tomlparse)

(defconst arb--project-marker "arb.toml"
  "The manifest, at the root of an arb project.")

;;; Base-36 ids (shared namespace across notes and subnodes)

(defun arb--base36-parse (s)
  "Parse base-36 string S into an integer."
  (math-read-number (concat "36#" s)))

(defun arb--base36-format (n)
  "Format integer N as a zero-padded four-digit base-36 string."
  (let* ((calc-number-radix 36)
         (digits (downcase (substring (math-format-number n) 3 nil))))
    (if (< (length digits) 4)
        (concat (make-string (- 4 (length digits)) ?0) digits)
      digits)))

;;; Project discovery and config

(defun arb--project-root (&optional directory)
  "Return the arb project root containing DIRECTORY.
DIRECTORY defaults to `default-directory'. Signal `user-error' when not
inside an arb project."
  (let ((root (locate-dominating-file (or directory default-directory)
                                      arb--project-marker)))
    (unless root
      (user-error "Not inside an arb project (no %s in this or any parent directory)"
                  arb--project-marker))
    (file-name-as-directory (expand-file-name root))))

(defun arb--source-globs (&optional root)
  "Return the source glob strings from ROOT's manifest."
  (let* ((manifest (tomlparse-file
                    (expand-file-name arb--project-marker
                                      (or root (arb--project-root)))
                    :object-type 'alist))
         (sources (alist-get 'sources manifest)))
    (unless (and (vectorp sources) (seq-every-p #'stringp sources))
      (user-error "%s: sources must be an array of strings"
                  arb--project-marker))
    (append sources nil)))


;;; Scanning

(defvar arb--scan-file-cache (make-hash-table :test 'equal)
  "Cache of on-disk arb scans.
Keys are absolute file names.  Values are plists with `:stamp' and
`:nodes'.  The stamp is derived from file attributes, so saved files are
rescanned lazily while unchanged files are reused across project scans.")

(defvar-local arb--buffer-scan-cache nil
  "Buffer-local cache of tree-sitter scan data.
Invalidated by `buffer-chars-modified-tick'.  This is for point-local
commands that repeatedly ask about the current buffer; project-wide scans
use `arb--scan-file-cache' because they read from disk.")

(defun arb-clear-cache ()
  "Clear arb's in-memory scan caches."
  (interactive)
  (clrhash arb--scan-file-cache)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq arb--buffer-scan-cache nil)))
  (message "arb: cleared scan caches"))

(defun arb--file-stamp (file)
  "Return a cache stamp for FILE based on its file attributes."
  (let ((attrs (file-attributes file)))
    (unless attrs
      (user-error "No such file: %s" file))
    (list (file-attribute-modification-time attrs)
          (file-attribute-size attrs))))

(defun arb--normalize (string)
  "Collapse whitespace in STRING for display."
  (replace-regexp-in-string "[ \t\n]+" " " (string-trim string)))

(defun arb--treesit-root ()
  "Return the Typst tree-sitter root node for the current buffer."
  (unless (treesit-parser-list nil 'typst t)
    (treesit-parser-create 'typst))
  (treesit-buffer-root-node 'typst))

(defun arb--treesit-walk (node fn)
  "Call FN on NODE and every descendant."
  (funcall fn node)
  (dotimes (i (treesit-node-child-count node))
    (arb--treesit-walk (treesit-node-child node i) fn)))

(defun arb--treesit-named-children (node)
  "Return NODE's named children."
  (treesit-node-children node t))

(defun arb--treesit-node-text (node)
  "Return NODE's source text without text properties."
  (treesit-node-text node t))

(defun arb--treesit-string-value (node)
  "Return the unquoted source value of Typst string NODE."
  (when (and node (string= (treesit-node-type node) "string"))
    (let ((text (arb--treesit-node-text node)))
      (if (and (>= (length text) 2)
               (eq (aref text 0) ?\")
               (eq (aref text (1- (length text))) ?\"))
          (substring text 1 -1)
        text))))

(defun arb--treesit-content-text (node)
  "Return the source text inside Typst content NODE's outer brackets."
  (when (and node (string= (treesit-node-type node) "content"))
    (let ((text (arb--treesit-node-text node)))
      (if (and (>= (length text) 2)
               (eq (aref text 0) ?\[)
               (eq (aref text (1- (length text))) ?\]))
          (substring text 1 -1)
        text))))

(defun arb--treesit-call-head (call)
  "Return CALL's innermost function-call header.
For a Typst body application like `subnode(...)[body]', the outer `call'
has the header call as its item and the body content as a later child."
  (let ((item (treesit-node-child-by-field-name call "item")))
    (if (and item (string= (treesit-node-type item) "call"))
        (arb--treesit-call-head item)
      call)))

(defun arb--treesit-call-name (call)
  "Return CALL's callee name as source text, or nil."
  (let* ((head (arb--treesit-call-head call))
         (item (treesit-node-child-by-field-name head "item")))
    (when item
      (arb--treesit-node-text item))))

(defun arb--treesit-call-group (call)
  "Return CALL's argument group node, or nil."
  (let ((head (arb--treesit-call-head call)))
    (seq-find (lambda (child) (string= (treesit-node-type child) "group"))
              (arb--treesit-named-children head))))

(defun arb--treesit-call-body (call)
  "Return CALL's trailing content body node, or nil.
This is the `[body]' in `#subnode(...)[body]' or `#link-node(...)[body]',
not a positional content argument inside the parenthesized group."
  (let ((head (arb--treesit-call-head call)))
    (seq-find (lambda (child)
                (and (string= (treesit-node-type child) "content")
                     (>= (treesit-node-start child) (treesit-node-end head))))
              (arb--treesit-named-children call))))

(defun arb--treesit-arguments (call)
  "Return the named syntax nodes inside CALL's parenthesized argument group."
  (when-let* ((group (arb--treesit-call-group call)))
    (seq-filter (lambda (child)
                  (not (string= (treesit-node-type child) "tagged")))
                (arb--treesit-named-children group))))

(defun arb--treesit-named-argument (call name)
  "Return the value node for CALL's named argument NAME, or nil."
  (when-let* ((group (arb--treesit-call-group call)))
    (seq-some (lambda (child)
                (when (string= (treesit-node-type child) "tagged")
                  (let ((field (treesit-node-child-by-field-name child "field")))
                    (when (and field (string= (arb--treesit-node-text field) name))
                      (seq-find (lambda (grandchild)
                                  (not (treesit-node-eq grandchild field)))
                                (arb--treesit-named-children child))))))
              (arb--treesit-named-children group))))

(defun arb--treesit-string-argument (call name)
  "Return CALL's named string argument NAME, or nil."
  (arb--treesit-string-value (arb--treesit-named-argument call name)))

(defun arb--treesit-tags-argument (call)
  "Return CALL's `tags: (...)' string values, or nil when absent."
  (when-let* ((tags-node (arb--treesit-named-argument call "tags")))
    (let (tags)
      (arb--treesit-walk
       tags-node
       (lambda (node)
         (when (string= (treesit-node-type node) "string")
           (push (arb--treesit-string-value node) tags))))
      (nreverse tags))))

(defun arb--treesit-node-plist (call kind &optional file)
  "Build an arb node plist for Typst CALL of KIND in FILE."
  (let* ((args (arb--treesit-arguments call))
         (id (arb--treesit-string-value (nth 0 args)))
         (title-node (nth 1 args))
         (title (and title-node
                     (arb--normalize
                      (arb--treesit-content-text title-node))))
         (taxon (or (arb--treesit-string-argument call "taxon") "note"))
         (tags (arb--treesit-tags-argument call))
         (supersedes (arb--treesit-string-argument call "supersedes"))
         (head (arb--treesit-call-head call))
         (plist (list :id id :title title :taxon taxon :tags tags
                      :file file :pos (treesit-node-start head) :kind kind)))
    (when supersedes
      (setq plist (plist-put plist :supersedes supersedes)))
    plist))

(defun arb--treesit-node-calls ()
  "Return Typst calls that define arb nodes in the current buffer."
  (let (calls)
    (arb--treesit-walk
     (arb--treesit-root)
     (lambda (node)
       (when (string= (treesit-node-type node) "call")
         (let ((name (arb--treesit-call-name node)))
           (cond
            ((string= name "node.with")
             (push (cons node 'note) calls))
            ((and (string= name "subnode")
                  (arb--treesit-call-body node))
             (push (cons node 'subnode) calls)))))))
    (nreverse calls)))

(defun arb--buffer-scan-data ()
  "Return cached node/link range data for the current buffer."
  (let ((tick (buffer-chars-modified-tick)))
    (if (and arb--buffer-scan-cache
             (eq tick (plist-get arb--buffer-scan-cache :tick)))
        arb--buffer-scan-cache
      (let ((file buffer-file-name)
            node-ranges
            link-ranges)
        (arb--treesit-walk
         (arb--treesit-root)
         (lambda (node)
           (when (string= (treesit-node-type node) "call")
             (let ((name (arb--treesit-call-name node)))
               (cond
                ((string= name "link-node")
                 (let* ((head (arb--treesit-call-head node))
                        (args (arb--treesit-arguments node))
                        (id (arb--treesit-string-value (car args))))
                   (when id
                     (push (list :id id
                                 :start (treesit-node-start head)
                                 :end (treesit-node-end node))
                           link-ranges))))
                ((or (string= name "node.with")
                     (and (string= name "subnode")
                          (arb--treesit-call-body node)))
                 (let* ((kind (if (string= name "node.with") 'note 'subnode))
                        (plist (arb--treesit-node-plist node kind file))
                        (head (arb--treesit-call-head node))
                        (start (treesit-node-start head))
                        (end (if (eq kind 'note)
                                 (point-max)
                               (treesit-node-end node))))
                   (push (list :node plist :start start :end end)
                         node-ranges))))))))
        (setq arb--buffer-scan-cache
              (list :tick tick
                    :node-ranges (nreverse node-ranges)
                    :link-ranges (nreverse link-ranges)))))))

(defun arb--scan-file-uncached (file)
  "Return the list of node plists defined in FILE without consulting caches."
  (with-temp-buffer
    (insert-file-contents file)
    (let (nodes)
      (dolist (entry (arb--treesit-node-calls))
        (pcase-let ((`(,call . ,kind) entry))
          (push (arb--treesit-node-plist call kind file) nodes)))
      (nreverse nodes))))

(defun arb--scan-file (file)
  "Return the list of node plists defined in FILE.
Results are cached per file and invalidated when FILE's modification time
or size changes.  The returned plists describe the on-disk contents; unsaved
buffer edits are intentionally not reflected in this project-wide cache."
  (let* ((file (expand-file-name file))
         (stamp (arb--file-stamp file))
         (cached (gethash file arb--scan-file-cache)))
    (if (equal stamp (plist-get cached :stamp))
        (plist-get cached :nodes)
      (let ((nodes (arb--scan-file-uncached file)))
        (puthash file (list :stamp stamp :nodes nodes) arb--scan-file-cache)
        nodes))))

(defun arb--glob->regexp (glob)
  "Translate a restricted shell GLOB into an anchored regexp over full paths.
`**' matches any number of path components (including none); `*' matches a
run of non-slash characters; `?' matches one non-slash character. Other
characters are matched literally."
  (let ((out "")
        (i 0)
        (n (length glob)))
    (while (< i n)
      (let ((c (aref glob i)))
        (cond
         ((and (eq c ?*) (< (1+ i) n) (eq (aref glob (1+ i)) ?*))
          ;; `**/' collapses an optional run of directories; a trailing `**'
          ;; matches the rest of the path.
          (cond
           ((and (< (+ i 2) n) (eq (aref glob (+ i 2)) ?/))
            (setq out (concat out "\\(?:.*/\\)?")
                  i (+ i 3)))
           (t (setq out (concat out ".*")
                    i (+ i 2)))))
         ((eq c ?*) (setq out (concat out "[^/]*") i (1+ i)))
         ((eq c ??) (setq out (concat out "[^/]") i (1+ i)))
         (t (setq out (concat out (regexp-quote (char-to-string c)))
                  i (1+ i))))))
    (concat "\\`" out "\\'")))

(defun arb--expand-glob (glob)
  "Expand absolute GLOB to a list of matching files.
Unlike `file-expand-wildcards', this supports `**' as a recursive
wildcard matching any number of intervening directories, matching the
`file/glob' semantics the manifest's globs are written against. The
wrapper reads these same globs, so the two must agree on `**'."
  (if (not (string-match-p "\\*\\*" glob))
      (file-expand-wildcards glob t)
    ;; Walk the tree from the static prefix (everything before the first
    ;; wildcard) and keep the files whose full path matches the glob.
    (let ((base (file-name-directory
                 (substring glob 0 (string-match "[*?]" glob))))
          (regexp (arb--glob->regexp glob)))
      (when (file-directory-p base)
        (seq-filter (lambda (file) (string-match-p regexp file))
                    (directory-files-recursively base ".*"))))))

(defun arb--source-files (&optional root)
  "Return the list of note source files to scan for ROOT.
Source globs are read from the manifest.

TODO: Ask `arb sources' instead of globbing here, once arb starts fast
enough to run on every command."
  (let ((root (arb--project-root root)))
    (delete-dups
     (sort
      (apply #'append
             (mapcar (lambda (glob)
                       (arb--expand-glob (expand-file-name glob root)))
                     (arb--source-globs root)))
      #'string<))))

(defun arb-nodes (&optional root)
  "Scan all note sources and return a flat list of node plists.
Each plist has :id, :title, :taxon, :file, :pos, and :kind."
  (apply #'append (mapcar #'arb--scan-file (arb--source-files root))))

(defun arb--next-id (&optional root)
  "Return the next free four-digit base-36 id across all nodes in ROOT."
  (let ((highest 0))
    (dolist (node (arb-nodes root))
      (let ((id (plist-get node :id)))
        (when (string-match-p "\\`[0-9a-z]\\{4\\}\\'" id)
          (setq highest (max highest (arb--base36-parse id))))))
    (arb--base36-format (1+ highest))))

;;; Completion

(defun arb--candidate (node)
  "Return the completion candidate string for NODE.
The visible text is the title; the id, taxon, and tags are appended as
an `invisible' suffix. The characters stay in the string, so completion
styles match on them (type an id, taxon, or tag to narrow) and the hash
lookup resolves, but the minibuffer hides text carrying a non-nil
`invisible' property, so the displayed list stays titles only. Including
the id makes every candidate unique, which is why identically-titled
nodes no longer need a visible \"(id)\" suffix to disambiguate."
  (let ((title (plist-get node :title))
        (id (plist-get node :id))
        (taxon (plist-get node :taxon))
        (tags (plist-get node :tags)))
    (concat title
            (propertize (concat " " id " " taxon
                                (and tags (concat " " (string-join tags " "))))
                        'invisible t))))

(defun arb--candidate-table (&optional root)
  "Return a hash table mapping candidate strings to node plists for ROOT.
See `arb--candidate' for the candidate format."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node (arb-nodes root))
      (puthash (arb--candidate node) node table))
    table))

(defun arb--affixation-function (table &optional root)
  "Return an affixation function over TABLE: taxon prefix, file suffix."
  (let ((root (arb--project-root root)))
    (lambda (candidates)
      (mapcar
       (lambda (candidate)
         (let* ((node (gethash candidate table))
                (taxon (plist-get node :taxon))
                (file (file-relative-name (plist-get node :file) root)))
           (list candidate
                 (propertize (format "%-11s " taxon)
                             'face 'completions-annotations)
                 (propertize (concat "  " file)
                             'face 'completions-annotations))))
       candidates))))

(defun arb--read-node (prompt &optional initial-input)
  "Prompt with PROMPT for a node and return its plist.
When INITIAL-INPUT is non-nil, prefill the minibuffer with it."
  (let* ((root (arb--project-root))
         (table (arb--candidate-table root))
         (affix (arb--affixation-function table root))
         (collection
          (lambda (string predicate action)
            (if (eq action 'metadata)
                `(metadata (category . arb-node)
                           (affixation-function . ,affix))
              (complete-with-action action table string predicate))))
         (choice (completing-read prompt collection nil t initial-input)))
    (gethash choice table)))

(defun arb--visit (node)
  "Visit NODE's file and move point to its definition."
  (find-file (plist-get node :file))
  (goto-char (plist-get node :pos)))

(defun arb--link-id-at-point ()
  "Return the target id of the `#link-node' at point, or nil.
Looks for a cached tree-sitter `link-node' range enclosing point first, then
falls back to the nearest link beginning before point."
  (save-excursion
    (save-restriction
      (widen)
      (let ((target (point))
            (containing nil)
            (containing-width nil)
            (previous nil)
            (previous-start -1))
        (dolist (range (plist-get (arb--buffer-scan-data) :link-ranges))
          (let ((id (plist-get range :id))
                (start (plist-get range :start))
                (end (plist-get range :end)))
            (when id
              (when (and (<= start target) (<= target end))
                (let ((width (- end start)))
                  (when (or (not containing-width) (< width containing-width))
                    (setq containing id
                          containing-width width))))
              (when (and (<= start target) (> start previous-start))
                (setq previous id
                      previous-start start)))))
        (or containing previous)))))

(defun arb--node-at-point ()
  "Return the plist of the innermost node or subnode enclosing point.
A top-level `node.with' is the file root fallback; each `#subnode(...)[...]'
encloses its declaration through the end of its body."
  (save-excursion
    (save-restriction
      (widen)
      (let ((target (point))
            (root nil)
            (best nil)
            (best-beg -1))
        (dolist (range (plist-get (arb--buffer-scan-data) :node-ranges))
          (let* ((plist (plist-get range :node))
                 (kind (plist-get plist :kind))
                 (beg (plist-get range :start))
                 (end (plist-get range :end)))
            (cond
             ((eq kind 'note)
              (unless root
                (setq root plist)))
             ((and (<= beg target) (<= target end) (> beg best-beg))
              (setq best plist
                    best-beg beg)))))
        (or best root)))))


;;; Versioning

(defun arb--node-by-id (id &optional root)
  "Return the node plist for ID in ROOT, or nil."
  (seq-find (lambda (node) (equal (plist-get node :id) id))
            (arb-nodes (or root (arb--project-root)))))

(defun arb--with-leading-hash-start (pos)
  "Return POS adjusted left to include a preceding Typst `#', if present."
  (if (and (> pos (point-min)) (eq (char-before pos) ?#))
      (1- pos)
    pos))

(defun arb--treesit-parent-of-type (node type)
  "Return NODE's nearest parent whose tree-sitter type is TYPE."
  (let ((parent (treesit-node-parent node))
        (found nil))
    (while (and parent (not found))
      (if (string= (treesit-node-type parent) type)
          (setq found parent)
        (setq parent (treesit-node-parent parent))))
    found))

(defun arb--version-arg-sources (call)
  "Return CALL's named argument source strings, except `supersedes'."
  (when-let* ((group (arb--treesit-call-group call)))
    (delq nil
          (mapcar (lambda (child)
                    (when (string= (treesit-node-type child) "tagged")
                      (let ((field (treesit-node-child-by-field-name child "field")))
                        (unless (and field
                                     (string= (arb--treesit-node-text field)
                                              "supersedes"))
                          (arb--treesit-node-text child)))))
                  (arb--treesit-named-children group)))))

(defun arb--version-node-source-info (node)
  "Return source/range details for NODE's definition."
  (let ((id (plist-get node :id))
        (file (plist-get node :file))
        found)
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (dolist (entry (arb--treesit-node-calls))
            (pcase-let ((`(,call . ,kind) entry))
              (let ((plist (arb--treesit-node-plist call kind file)))
                (when (and (not found) (equal (plist-get plist :id) id))
                  (let* ((head (arb--treesit-call-head call))
                         (args (arb--treesit-arguments call))
                         (title-node (nth 1 args))
                         (body-node (arb--treesit-call-body call))
                         (head-start (treesit-node-start head))
                         (definition-start (arb--with-leading-hash-start head-start))
                         (definition-end (treesit-node-end call))
                         (show-node (and (eq kind 'note)
                                         (arb--treesit-parent-of-type call "show")))
                         (show-start (and show-node
                                          (arb--with-leading-hash-start
                                           (treesit-node-start show-node))))
                         (show-end (and show-node (treesit-node-end show-node))))
                    (setq found
                          (list :buffer (current-buffer)
                                :file file
                                :kind kind
                                :id id
                                :title-source (and title-node
                                                   (arb--treesit-node-text title-node))
                                :arg-sources (arb--version-arg-sources call)
                                :body-source (and body-node
                                                  (arb--treesit-node-text body-node))
                                :definition-start (or show-start definition-start)
                                :definition-end (or show-end definition-end)
                                :call-start definition-start
                                :call-end definition-end))))))))))
    (unless found
      (user-error "Could not find node %s in %s" id file))
    found))

(defun arb--version-format-items (id title-source arg-sources old-id)
  "Return formatted call items for a new version of OLD-ID with ID."
  (append (list (format "\"%s\"" id) title-source)
          arg-sources
          (list (format "supersedes: \"%s\"" old-id))))

(defun arb--version-format-show (id title-source arg-sources old-id)
  "Return a `#show: node.with' source block for a new version."
  (concat "#show: node.with(\n  "
          (string-join (arb--version-format-items id title-source arg-sources old-id)
                       ",\n  ")
          "\n)"))

(defun arb--version-body-point-offset (header body-source)
  "Return the point offset into HEADER + BODY-SOURCE for editing the body."
  (let ((offset (1+ (length header))))
    (when (and (> (length body-source) 1)
               (eq (aref body-source 1) ?\n))
      (setq offset (1+ offset)))
    offset))

(defun arb--version-next-ids (root count)
  "Return COUNT fresh consecutive ids in ROOT."
  (let ((first (arb--base36-parse (arb--next-id root)))
        ids)
    (dotimes (i count)
      (push (arb--base36-format (+ first i)) ids))
    (nreverse ids)))

(defun arb--file-in-directory-p (file directory)
  "Return non-nil when FILE is in DIRECTORY."
  (file-in-directory-p (file-truename file) (file-truename directory)))

(defun arb--barb-source-root (root)
  "Return Barb's source directory in ROOT."
  (file-name-as-directory (expand-file-name "subtrees/barb/source" root)))

(defun arb--barb-live-subnode-p (node root)
  "Return non-nil when NODE is a live Barb subnode."
  (let* ((file (plist-get node :file))
         (source (arb--barb-source-root root))
         (scratch (expand-file-name "Scratch" source)))
    (and (eq (plist-get node :kind) 'subnode)
         (arb--file-in-directory-p file source)
         (not (arb--file-in-directory-p file scratch)))))

(defun arb--top-level-note-p (node root)
  "Return non-nil when NODE is a top-level note under notes/."
  (and (eq (plist-get node :kind) 'note)
       (arb--file-in-directory-p
        (plist-get node :file)
        (expand-file-name "notes" root))))

(defun arb--barb-module-path (live-file root)
  "Return the Agda module path for LIVE-FILE under Barb source."
  (let* ((relative (file-relative-name live-file (arb--barb-source-root root)))
         (without-ext (replace-regexp-in-string "\\.lagda\\.typ\\'" "" relative)))
    (replace-regexp-in-string "/" "." without-ext)))

(defun arb--barb-scratch-file (live-file root)
  "Return the Scratch mirror file path for LIVE-FILE."
  (expand-file-name (file-relative-name live-file (arb--barb-source-root root))
                    (expand-file-name "Scratch" (arb--barb-source-root root))))

(defun arb--barb-open-import-lines (live-file)
  "Return all Agda `open import' lines in LIVE-FILE."
  (with-temp-buffer
    (insert-file-contents live-file)
    (goto-char (point-min))
    (let (lines)
      (while (re-search-forward "^open import .+$" nil t)
        (push (match-string-no-properties 0) lines))
      (nreverse lines))))

(defun arb--barb-create-scratch-file (scratch-file scratch-id module imports)
  "Create SCRATCH-FILE with SCRATCH-ID, MODULE, and IMPORTS."
  (make-directory (file-name-directory scratch-file) t)
  (with-temp-file scratch-file
    (insert (format "#import \"@local/notes:0.1.0\": *\n#import \"/hott.typ\": *\n\n#show: node.with(\"%s\", [`Scratch.%s`], taxon: \"module\")\n```agda\nmodule Scratch.%s where\n\n"
                    scratch-id module module))
    (when imports
      (insert (string-join imports "\n") "\n"))
    (insert "```\n\n")))

(defun arb--barb-add-aggregator-import (root scratch-module)
  "Ensure Barb.lagda.typ imports SCRATCH-MODULE."
  (let ((file (expand-file-name "Barb.lagda.typ" (arb--barb-source-root root)))
        (line (concat "import " scratch-module)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (unless (re-search-forward (concat "^" (regexp-quote line) "$") nil t)
            (goto-char (point-min))
            (let (imports beg end)
              (while (re-search-forward "^import Scratch\\..+$" nil t)
                (unless beg
                  (setq beg (line-beginning-position)))
                (setq end (line-end-position))
                (push (match-string-no-properties 0) imports))
              (push line imports)
              (setq imports (sort (delete-dups imports) #'string<))
              (if (and beg end)
                  (progn
                    (delete-region beg end)
                    (goto-char beg)
                    (insert (string-join imports "\n")))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert line))))))
      (let ((make-backup-files nil))
        (save-buffer)))))

(defun arb--append-node-source (file source)
  "Append SOURCE as a node block to FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (unless (looking-back "\n\n" nil) (insert "\n"))
      (insert source)
      (unless (bolp) (insert "\n"))
      (let ((make-backup-files nil))
        (save-buffer)))))

(defun arb--version-non-barb-note (node root)
  "Create a new top-level version of NODE in ROOT."
  (let* ((old-id (plist-get node :id))
         (new-id (car (arb--version-next-ids root 1)))
         (info (arb--version-node-source-info node))
         (path (expand-file-name (concat new-id ".typ")
                                 (expand-file-name "notes" root)))
         (show-source (arb--version-format-show
                       new-id
                       (plist-get info :title-source)
                       (plist-get info :arg-sources)
                       old-id)))
    (when (file-exists-p path)
      (user-error "Note file already exists: %s" path))
    (let (body-start)
      (with-current-buffer (plist-get info :buffer)
        (let ((prelude (buffer-substring-no-properties
                        (point-min) (plist-get info :definition-start)))
              (body (buffer-substring-no-properties
                     (plist-get info :definition-end) (point-max))))
          (setq body-start (+ (point-min) (length prelude) (length show-source)))
          (with-temp-file path
            (insert prelude show-source body))))
      (find-file path)
      (goto-char body-start))
    (message "arb: created %s superseding %s" new-id old-id)))

(defun arb--version-barb-subnode (node root)
  "Create a new live Barb version of subnode NODE in ROOT."
  (let* ((old-id (plist-get node :id))
         (live-file (plist-get node :file))
         (scratch-file (arb--barb-scratch-file live-file root))
         (scratch-exists (file-exists-p scratch-file))
         (ids (arb--version-next-ids root (if scratch-exists 1 2)))
         (new-id (car ids))
         (scratch-id (cadr ids))
         (module (arb--barb-module-path live-file root))
         (scratch-module (concat "Scratch." module))
         (info (arb--version-node-source-info node))
         (old-source (with-current-buffer (plist-get info :buffer)
                       (buffer-substring-no-properties
                        (plist-get info :call-start)
                        (plist-get info :call-end))))
         (body-source (plist-get info :body-source))
         (new-header (concat "#subnode(\n  "
                             (string-join
                              (arb--version-format-items
                               new-id
                               (plist-get info :title-source)
                               (plist-get info :arg-sources)
                               old-id)
                              ",\n  ")
                             "\n)"))
         (new-source (concat new-header body-source))
         (point-offset (arb--version-body-point-offset new-header body-source)))
    (unless scratch-exists
      (arb--barb-create-scratch-file
       scratch-file scratch-id module (arb--barb-open-import-lines live-file)))
    (arb--barb-add-aggregator-import root scratch-module)
    (arb--append-node-source scratch-file old-source)
    (with-current-buffer (plist-get info :buffer)
      (goto-char (plist-get info :call-start))
      (delete-region (plist-get info :call-start) (plist-get info :call-end))
      (insert new-source)
      (let ((make-backup-files nil))
        (save-buffer))
      (goto-char (+ (plist-get info :call-start) point-offset))
      (switch-to-buffer (current-buffer)))
    (message "arb: created %s superseding %s; moved %s to %s"
             new-id old-id old-id scratch-file)))

(defun arb-version-node (node)
  "Create the next version of NODE and visit the new version's body."
  (interactive (list (arb--read-node "Version node: ")))
  (let ((root (arb--project-root)))
    (cond
     ((arb--barb-live-subnode-p node root)
      (arb--version-barb-subnode node root))
     ((arb--top-level-note-p node root)
      (arb--version-non-barb-note node root))
     (t
      (user-error "Don't know how to version node %s in %s"
                  (plist-get node :id)
                  (file-relative-name (plist-get node :file) root))))))

;;; Commands

(defun arb-find-note ()
  "Select a node and visit its definition."
  (interactive)
  (arb--visit (arb--read-node "Find node: ")))

(defun arb-insert-node ()
  "Select a node and insert a `#link-node' to it at point.
With an active region, use it as the link description; if the region
matches the node title (or there is no region), insert the wiki-style
bare `#link-node(\"id\")'."
  (interactive)
  (let* ((region (when (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
         (initial-input (and region (arb--normalize region)))
         (node (arb--read-node "Insert link to node: " initial-input))
         (id (plist-get node :id))
         (title (plist-get node :title))
         (description (and region (not (string= region title)) region)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (if description
                (format "#link-node(\"%s\")[%s]" id description)
              (format "#link-node(\"%s\")" id)))))

(defun arb-goto-note-at-point ()
  "Follow the `#link-node' at point to its definition."
  (interactive)
  (let ((id (arb--link-id-at-point)))
    (unless id
      (user-error "No #link-node at point"))
    (let ((node (seq-find (lambda (n) (equal (plist-get n :id) id))
                          (arb-nodes (arb--project-root)))))
      (unless node
        (user-error "No node with id %s" id))
      (arb--visit node))))

(defun arb-new-note (title)
  "Create a new note under notes/ titled TITLE and visit it."
  (interactive "sNote title: ")
  (let* ((root (arb--project-root))
         (id (arb--next-id root))
         (dir (expand-file-name "notes" root))
         (path (expand-file-name (concat id ".typ") dir)))
    (when (file-exists-p path)
      (user-error "Note file already exists: %s" path))
    (with-temp-file path
      (insert (format (concat "#import \"@local/notes:0.1.0\": *\n\n"
                              "#show: node.with(\"%s\", [%s])\n\n")
                      id title)))
    (find-file path)
    (goto-char (point-max))))

(defun arb-insert-subnode (title)
  "Insert a `#subnode' skeleton titled TITLE at point, with a fresh id.
Leave the initial body line unindented so literate Agda code blocks can stay
flush-left; prose can still be indented manually or by `typst-ts-mode'."
  (interactive "sSubnode title: ")
  (let ((id (arb--next-id (arb--project-root))))
    (insert (format "#subnode(\"%s\", [%s])[\n\n]\n" id title))
    (forward-line -2)
    (end-of-line)))

;;; Indentation

(defconst arb--raw-block-indent-rule
  '((node-is "raw_blck") column-0 0)
  "Tree-sitter indentation rule that keeps Typst raw blocks at column zero.")

(defun arb-flush-raw-blocks-in-lagda ()
  "Keep raw blocks flush-left in `.lagda.typ' buffers of arb projects.

Agda's layout checker sees the contents of every Typst raw block in a
`.lagda.typ' file.  Top-level declarations from different blocks must start
in the same column, so code blocks should stay at column zero even when they
occur inside indented `#subnode' prose.  This prepends a buffer-local
`raw_blck' indentation rule while leaving prose, math, lists, and theorem
bodies on the normal `typst-ts-indent-offset' path."
  (when (and buffer-file-name
             (string-match-p "\\.lagda\\.typ\\'" buffer-file-name)
             (locate-dominating-file buffer-file-name arb--project-marker))
    (setq-local
     treesit-simple-indent-rules
     (mapcar
      (lambda (entry)
        (if (and (eq (car entry) 'typst)
                 (not (member arb--raw-block-indent-rule (cdr entry))))
            (cons 'typst (cons arb--raw-block-indent-rule (cdr entry)))
          entry))
      treesit-simple-indent-rules))))

(add-hook 'typst-ts-mode-hook #'arb-flush-raw-blocks-in-lagda)

(provide 'arb)
