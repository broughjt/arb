#lang typed/racket/base

(require racket/cmdline
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/system
         typed/file/glob)

;; Typed Racket has no type for `path<?`.
(require/typed racket/base [path<? (-> Path Path Boolean)])
(require/typed toml [parse-toml (-> String (HashTable Symbol Any))])

(define manifest-name "arb.toml")

(module+ main
  (match (parse-arguments (current-command-line-arguments))
    [(build-subcommand) (build (find-root (current-directory)))]))

(define-type Subcommand (U build-subcommand))
(struct build-subcommand () #:transparent)

(: parse-arguments (-> (Vectorof String) Subcommand))
(define (parse-arguments argv)
  (command-line
   #:program "arb"
   #:argv argv
   #:usage-help
   "Commands:"
   "  build  build the site into build/"
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
    [else (raise-user-error 'arb
                            "unknown command ~a; expected build"
                            subcommand)]))

(: parse-build-subcommand (-> String (Listof String) build-subcommand))
(define (parse-build-subcommand program arguments)
  (command-line #:program program
                #:argv arguments
                #:args ()
                (build-subcommand)))

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
  (display-lines-to-file (map path->string sources) path))

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
