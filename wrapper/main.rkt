#lang typed/racket/base

(require racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/set
         racket/system
         typed/file/glob
         typed/json)

;; Typed Racket has no type for `path<?`.
(require/typed racket/base [path<? (-> Path Path Boolean)])
(require/typed toml [parse-toml (-> String (HashTable Symbol Any))])
;; Typed Racket's type for `subprocess` does not allow the error output to be
;; merged into the standard output.
(require/typed racket/base
               [(subprocess subprocess/merged)
                (-> #f #f 'stdout Path String *
                    (Values Subprocess Input-Port Output-Port #f))])

;; Typed Racket cannot type `ffi/unsafe`, and Racket cannot make a FIFO.
(module fifo racket/base
  (require ffi/unsafe)
  (provide make-fifo)
  (define mkfifo
    (get-ffi-obj "mkfifo" #f (_fun #:save-errno 'posix _path _int -> _int)))
  (define (make-fifo path)
    (unless (zero? (mkfifo path #o600))
      (error 'arb "cannot make the FIFO ~a (errno ~a)" path (saved-errno)))))
(require/typed 'fifo [make-fifo (-> Path-String Void)])

(define manifest-name "arb.toml")

(module+ main
  ;; TODO: Use command line parsing similar to the handoff script (see
  ;; ~/repositories/handoff) when more than one subcommand is added
  (match (current-command-line-arguments)
    [(vector "build") (build (find-root (current-directory)))]
    [(vector "watch") (watch (find-root (current-directory)))]
    [_ (raise-user-error "usage: arb build | watch")]))

;; Typst refuses to read files outside a fixed root (e.g. the project root), so
;; we need to put non-source inputs to `typst compile` or `typst watch` in an
;; intermediate directory reachable within the fixed root. We use `.arb/` for
;; this directory.
;;
;; The `site.typ` file expects a `.arb/sources.txt` file which contains a
;; newline-separated list of source paths and an `.arb/agda-annotations.json`
;; file that contains the Agda annotations. We first build into a staging
;; directory inside `.arb/` and then rename it to `build/` at the end, so a
;; failed build leaves the previous one alone and no stale pages survive.
;;
;; The `.arb/` directory is kept between builds, since it holds the lock.
;; Only `.arb/staging` and `.arb/types` must start out empty: Typst never
;; deletes an output it has stopped emitting, and the hover file of a deleted
;; module would otherwise be copied into the site.
(: build (-> Path Void))
(define (build root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (lock-checkout)
    (delete-directory/files ".arb/staging" #:must-exist? #f)
    (delete-directory/files ".arb/types" #:must-exist? #f)
    (write-sources sources ".arb/sources.txt")
    (putenv "LC_ALL" "C.UTF-8")
    (stop-on-failure
     "agda-annotate"
     (capture-failure
      "agda-annotate"
      (list* "-o"
             ".arb"
             (filter (curryr path-has-extension? #".lagda.typ") sources))))
    (run "typst"
         (list "compile"
               "--root" "."
               "--features" "bundle,html"
               "--format" "bundle"
               "site.typ"
               ".arb/staging"))
    (stop-on-failure "pagefind" (finish-staging))
    (delete-directory/files "build" #:must-exist? #f)
    (rename-file-or-directory ".arb/staging" "build")))

;; With `--color always`, `typst watch` writes this before every status, to a
;; pipe as well as to a terminal.
(define clear-screen #"\e[2J\e[1;1H")

;; Runs `typst watch` and owns the screen: on every change it clears the screen
;; and prints Typst's current block, which is whatever Typst wrote after its
;; last clear, and then pagefind's output if it failed. Typst's text is never
;; parsed. Whether a compile succeeded comes from the dependency record Typst
;; writes after each one, to a FIFO, so that reading one opening of the FIFO to
;; its end is one finished compile. Each successful compile is published to
;; `build/`.
(: watch (-> Path Void))
(define (watch root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root]
                 [current-subprocess-custodian-mode 'kill])
    (lock-checkout)
    (write-sources sources ".arb/sources.txt")
    (display-to-file "{}" ".arb/agda-annotations.json" #:exists 'truncate)
    (delete-directory/files ".arb/types" #:must-exist? #f)
    (make-directory ".arb/types")

    ;; The FIFO is opened before Typst starts. Opening it for reading does not
    ;; wait for a writer, but Typst's open for writing waits for a reader, so
    ;; no record is lost while the reader is between openings.
    (define fifo ".arb/deps")
    (delete-directory/files fifo #:must-exist? #f)
    (make-fifo fifo)
    (define compiles : (Channelof JSExpr) (make-channel))
    (define first-opening (open-input-file fifo))
    (thread (λ ()
              (let loop : Void ([opening : Input-Port first-opening])
                (define record (port->bytes opening))
                (close-input-port opening)
                (channel-put compiles (bytes->jsexpr record))
                (loop (open-input-file fifo)))))

    (define-values (typst typst-port typst-input _error)
      (subprocess/merged #f #f 'stdout
                         (executable-path "typst")
                         "--color" "always"
                         "watch"
                         "--no-serve"
                         "--features" "bundle,html"
                         "--format" "bundle"
                         "--root" "."
                         "--deps" fifo
                         "--deps-format" "json"
                         "site.typ"
                         ".arb/staging"))
    (close-output-port typst-input)

    (define typst-output : Bytes #"")
    (define pagefind-failure : (U Bytes #f) #f)
    (define chunk (make-bytes 65536))
    (: read-typst-output (-> Void))
    (define (read-typst-output)
      (define count (read-bytes-avail!* chunk typst-port))
      (when (exact-positive-integer? count)
        (set! typst-output
              (after-last-clear
               (bytes-append typst-output (subbytes chunk 0 count))))))
    (: redraw (-> Void))
    (define (redraw)
      (write-bytes clear-screen)
      (write-bytes typst-output)
      (define failure pagefind-failure)
      (when failure
        (write-bytes #"pagefind failed\n")
        (write-bytes failure))
      (flush-output))

    (with-handlers ([exn:break? void])
      (dynamic-wind
       void
       (λ ()
         (let loop : Void ()
           (sync (handle-evt typst-port
                             (λ (_)
                               (read-typst-output)
                               (redraw)
                               (loop)))
                 (handle-evt compiles
                             (λ ([record : JSExpr])
                               (define outputs
                                 (and (hash? record) (hash-ref record 'outputs)))
                               (when (list? outputs)
                                 (set! pagefind-failure
                                       (publish (filter string? outputs)))
                                 (redraw))
                               (loop)))
                 (handle-evt typst
                             (λ (_)
                               (read-typst-output)
                               (redraw)
                               (raise-user-error 'arb "typst exited"))))))
       (λ ()
         (subprocess-kill typst #t)
         (delete-file fifo))))))

(: after-last-clear (-> Bytes Bytes))
(define (after-last-clear output)
  (define pattern (byte-regexp (bytes-append #"^.*" (regexp-quote clear-screen))))
  (assert (regexp-replace pattern output #"") bytes?))

;; Two commands at once in one checkout would trample `.arb/`, so each takes
;; an exclusive lock on `.arb/lock` in the current directory, creating `.arb/`
;; if needed, and fails if another holds it. The port is never closed: the
;; lock lasts until the process exits, when the OS releases it.
(: lock-checkout (-> Void))
(define (lock-checkout)
  (make-directory* ".arb")
  (define port (open-output-file ".arb/lock" #:exists 'truncate))
  (unless (port-try-file-lock? port 'exclusive)
    (raise-user-error 'arb "another arb command is running in this checkout")))

;; Adds to `.arb/staging` what Typst does not write: the hover files and the
;; search index. Returns pagefind's output if it failed.
(: finish-staging (-> (U Bytes #f)))
(define (finish-staging)
  (copy-directory/files ".arb/types" ".arb/staging/types")
  (capture-failure
   "pagefind"
   (list "--site" ".arb/staging"
         "--root-selector" ".node"
         "--exclude-selectors"
         ".node-meta, .node-rail, .backmatter, pre, .transclusion, math[display='block']")))

;; Publishes a successful compile, given the files it wrote: makes `build/`
;; hold exactly what a one-shot build would. Returns pagefind's output if it
;; failed.
;;
;; The copy into `build/` is not atomic, so a reader can see it half done.
;; Typst may also begin its next compile meanwhile, so a file can be copied
;; while Typst rewrites it; the publish of that compile repairs it.
(: publish (-> (Listof String) (U Bytes #f)))
(define (publish outputs)
  ;; Typst never deletes an output it has stopped emitting, and pagefind names
  ;; its files by content hash, so stale ones would pile up.
  (delete-directory/files ".arb/staging/types" #:must-exist? #f)
  (delete-directory/files ".arb/staging/pagefind" #:must-exist? #f)
  (define written (list->set outputs))
  (for ([file (in-list (files-in ".arb/staging"))])
    (define staged (build-path ".arb/staging" file))
    (unless (set-member? written (path->string staged))
      (delete-file staged)))
  (define pagefind-failure (finish-staging))
  (define staged (files-in ".arb/staging"))
  (for ([file (in-list staged)])
    (define published (build-path "build" file))
    (make-parent-directory* published)
    (copy-file (build-path ".arb/staging" file) published #:exists-ok? #t))
  (for ([file (in-list (files-in "build"))])
    (unless (member file staged)
      (delete-file (build-path "build" file))))
  pagefind-failure)

;; The files in a directory and beneath it, relative to it.
(: files-in (-> Path-String (Listof Path)))
(define (files-in directory)
  (parameterize ([current-directory directory])
    (for/list ([file (in-directory)] #:when (file-exists? file))
      file)))

(: find-root (-> Path Path))
(define (find-root directory)
  (define-values (parent _name _directory?) (split-path directory))
  (cond
    [(file-exists? (build-path directory manifest-name)) directory]
    [(path? parent) (find-root parent)]
    [else (raise-user-error 'arb
                            "no ~a in this directory or above it"
                            manifest-name)]))

(struct manifest ([source-globs : (Listof String)]))

(: read-manifest (-> Path manifest))
(define (read-manifest root)
  (define table
    (with-handlers ([exn:fail? (λ ([e : exn])
                                 (raise-user-error 'arb
                                                   "~a: ~a"
                                                   manifest-name
                                                   (exn-message e)))])
      (parse-toml (file->string (build-path root manifest-name)))))
  (define globs (hash-ref table 'sources #f))
  (if (and (list? globs) (andmap string? globs))
      (manifest globs)
      (raise-user-error 'arb
                        "~a must have a sources array of glob strings"
                        manifest-name)))

(: source-paths (-> Path manifest (Listof Path)))
(define (source-paths root manifest)
  (define paths
    (for*/list : (Listof Path)
               ([glob (in-list (manifest-source-globs manifest))]
                [path (in-glob (build-path root glob))])
      (assert (find-relative-path root path) path?)))
  (sort (remove-duplicates paths) path<?))

(: write-sources (-> (Listof Path) Path-String Void))
(define (write-sources sources path)
  (display-lines-to-file (map path->string sources) path #:exists 'truncate))

(: run (-> String (Listof Path-String) Void))
(define (run program arguments)
  (unless (apply system* (executable-path program) arguments)
    (raise-user-error 'arb "~a failed" program)))

;; Runs a program with its output captured, and returns the output if the
;; program failed.
(: capture-failure (-> String (Listof Path-String) (U Bytes #f)))
(define (capture-failure program arguments)
  (define output (open-output-bytes))
  (define succeeded?
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (apply system* (executable-path program) arguments)))
  (and (not succeeded?) (get-output-bytes output)))

(: stop-on-failure (-> String (U Bytes #f) Void))
(define (stop-on-failure program failure)
  (when failure
    (write-bytes failure (current-error-port))
    (raise-user-error 'arb "~a failed" program)))

(: executable-path (-> String Path))
(define (executable-path program)
  (or (find-executable-path program)
      (raise-user-error 'arb "~a is not on PATH" program)))
