#lang typed/racket/base

(require racket/cmdline
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/system
         typed/file/glob
         typed/json)

(require/typed racket/base [path<? (-> Path Path Boolean)])
(require/typed toml [parse-toml (-> String (HashTable Symbol Any))])

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
  (match (parse-arguments (current-command-line-arguments))
    [(build-subcommand) (build (find-root (current-directory)))]
    [(watch-subcommand) (watch (find-root (current-directory)))]))

(define-type Subcommand (U build-subcommand watch-subcommand))
(struct build-subcommand () #:transparent)
(struct watch-subcommand () #:transparent)

(: parse-arguments (-> (Vectorof String) Subcommand))
(define (parse-arguments argv)
  (command-line
   #:program "arb"
   #:argv argv
   #:usage-help
   "Commands:"
   "  build  build the site into build/"
   "  watch  rebuild the site whenever a source changes"
   #:args (subcommand . arguments)
   ;; Typed Racket types the formal arguments of `#:args` as `Any`, so we do a
   ;; runtime check explicitly (that we expect will never fail).
   (parse-subcommand (assert subcommand string?)
                     (map (λ (a) (assert a string?)) arguments))))

(: parse-subcommand (-> String (Listof String) Subcommand))
(define (parse-subcommand subcommand arguments)
  (define program (string-append "arb " subcommand))
  (case subcommand
    [("build") (parse-build-subcommand program arguments)]
    [("watch") (parse-watch-subcommand program arguments)]
    [else (raise-user-error 'arb
                            "unknown command ~a; expected build or watch"
                            subcommand)]))

(: parse-build-subcommand (-> String (Listof String) build-subcommand))
(define (parse-build-subcommand program arguments)
  (command-line #:program program
                #:argv arguments
                #:args ()
                (build-subcommand)))

(: parse-watch-subcommand (-> String (Listof String) watch-subcommand))
(define (parse-watch-subcommand program arguments)
  (command-line #:program program
                #:argv arguments
                #:args ()
                (watch-subcommand)))

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
;; TODO: Two builds at once in one checkout trample `.arb/`. Revisit whether
;; to take a lock per checkout when adding watch.
(: build (-> Path Void))
(define (build root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (delete-directory/files ".arb" #:must-exist? #f)
    (make-directory ".arb")
    (write-sources sources ".arb/sources.txt")
    (putenv "LC_ALL" "C.UTF-8")
    (run "agda-annotate"
         (list* "-o"
                ".arb"
                (filter (curryr path-has-extension? #".lagda.typ") sources))
         #:quiet? #t)
    (run "typst"
         (list "compile"
               "--root" "."
               "--features" "bundle,html"
               "--format" "bundle"
               "site.typ"
               ".arb/staging"))
    (copy-directory/files ".arb/types" ".arb/staging/types")
    (run "pagefind"
         (list "--site" ".arb/staging"
               "--root-selector" ".node"
               "--exclude-selectors"
               ".node-meta, .node-rail, .backmatter, pre, .transclusion, math[display='block']")
         #:quiet? #t)
    (delete-directory/files "build" #:must-exist? #f)
    (rename-file-or-directory ".arb/staging" "build")))

;; Runs `typst watch` with the terminal as its output, so Typst draws its own
;; status and diagnostics, and reads the record Typst writes after each compile.
(: watch (-> Path Void))
(define (watch root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (make-directory* ".arb")
    (write-sources sources ".arb/sources.txt")
    (display-to-file "{}" ".arb/agda-annotations.json" #:exists 'truncate)
    (putenv "LC_ALL" "C.UTF-8")

    ;; The `--deps` Typst option causes it to write the compilation dependencies
    ;; to a file. We use this to detect compilation iterations during `typst
    ;; watch`, since parsing its terminal output (on stderr) for this
    ;; information is quite tricky. Typst writes the dependencies as part of the
    ;; call to `compile_once` in the main watch loop in
    ;; crates/typst-cli/src/watch.rs. It writes them last, after the outputs and
    ;; the diagnostics, once per compile whether or not the compile succeeded.
    ;;
    ;; For each compile, Typst opens the path afresh (`File::create`), writes
    ;; one record, and closes it. Were the path a regular file, each record
    ;; would overwrite the last, nothing would tell us when one arrived, and we
    ;; could read one half rewritten. We make the path a FIFO instead: its
    ;; truncation does nothing, so the records queue up in order, the port
    ;; becomes ready when one arrives, and if we fall behind, Typst's writes
    ;; block rather than overwrite. (`--deps -` would need no FIFO, but Typst
    ;; never flushes its stdout, so the records would sit in its buffer.)
    ;;
    ;; Once the last writer of a FIFO closes, its reader sees EOF. We hold a
    ;; write end open ourselves, so the FIFO never reaches EOF and the records
    ;; arrive as one stream of JSON objects, which delimit themselves. (Hence
    ;; `--deps-format json`: the records of the `zero` format would run
    ;; together.) Reopening the read end after each EOF would instead leave a
    ;; window in which Typst could open the FIFO before we close the old read
    ;; end, and closing it would then lose the record or fail Typst's write.
    ;;
    ;; We open the read end first, because a non-blocking open of the write end
    ;; fails while there is no reader. So Typst's open, which waits for a
    ;; reader, never waits.
    (define dependencies-path ".arb/deps")
    (delete-directory/files dependencies-path #:must-exist? #f)
    (make-fifo dependencies-path)
    (define dependencies-port (open-input-file dependencies-path))
    (define dependencies-writer
      (open-output-file dependencies-path #:exists 'append))
    (define-values (typst _output _input _error)
      (subprocess (current-output-port)
                  (current-input-port)
                  (current-error-port)
                  (or (find-executable-path "typst")
                      (raise-user-error 'arb "typst is not on PATH"))
                  "watch"
                  "--no-serve"
                  "--features" "bundle,html"
                  "--format" "bundle"
                  "--root" "."
                  "--deps" dependencies-path
                  "--deps-format" "json"
                  "site.typ"
                  ".arb/staging"))

    ;; An interrupt from the terminal reaches Typst too, and ends the watch.
    (with-handlers ([exn:break? void])
      (dynamic-wind
       void
       (thunk
         (let loop : Void ()
           (sync (handle-evt dependencies-port
                             (λ (_)
                               (read-dependencies dependencies-port)
                               (loop)))
                 (handle-evt typst
                             (λ (_)
                               (raise-user-error 'arb "typst exited"))))))
       (thunk
         (subprocess-kill typst #t)
         (close-output-port dependencies-writer)
         (close-input-port dependencies-port)
         (delete-file dependencies-path))))))

;; Typst has a `--deps` option that writes the inputs and outputs of a compile
;; to a file. Paths under the root are relative to the current directory, and
;; paths outside it, such as files in the Typst package cache, are absolute. The
;; `outputs` field is `#f` if the compile failed. See `write_deps_json` in
;; Typst's crates/typst-cli/src/deps.rs.
(struct dependencies ([inputs : (Listof Path)]
                      [outputs : (U #f (Listof Path))]))

(: read-dependencies (-> Input-Port dependencies))
(define (read-dependencies port)
  ;; We opened the FIFO with a write end too, so the port will not ever yield
  ;; EOF.
  (define table (assert (read-json port) hash?))
  (define inputs (assert (hash-ref table 'inputs) list?))
  (define outputs (hash-ref table 'outputs))
  ;; Shouldn't we be making sure each element is a string and failing explicitly
  ;; otherwise?

  ;; And shouldn't we check that it is either #f or a list of strings, not
  ;; silently assume it was #f if it is not a list?
  (dependencies (map string->path (filter string? inputs))
                (and (list? outputs)
                     (map string->path (filter string? outputs)))))

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

(: run (->* (String (Listof Path-String)) (#:quiet? Boolean) Void))
(define (run program arguments #:quiet? [quiet? #f])
  (define executable
    (or (find-executable-path program)
        (raise-user-error 'arb "~a is not on PATH" program)))
  (define output (open-output-bytes))
  (define succeeded?
    (parameterize ([current-output-port (if quiet?
                                            output
                                            (current-output-port))]
                   [current-error-port (if quiet?
                                           output
                                           (current-error-port))])
      (apply system* executable arguments)))
  (unless succeeded?
    (write-bytes (get-output-bytes output) (current-error-port))
    (raise-user-error 'arb "~a failed" program)))
