#lang typed/racket/base

(require racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/promise
         racket/string
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

;; Each program is looked up on PATH once, when it is first run.
(define agda-annotate-executable
  (delay (or (find-executable-path "agda-annotate")
             (raise-user-error 'arb "agda-annotate is not on PATH"))))
(define typst-executable
  (delay (or (find-executable-path "typst")
             (raise-user-error 'arb "typst is not on PATH"))))
(define pagefind-executable
  (delay (or (find-executable-path "pagefind")
             (raise-user-error 'arb "pagefind is not on PATH"))))

(module+ main
  (match (parse-arguments (current-command-line-arguments))
    [(build-subcommand) (handle-build)]
    [(watch-subcommand) (handle-watch)]))

;; CLI parsing

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

;; Build and watch implementations

;; A value together with the times at which computing it started and ended, in
;; milliseconds as `current-inexact-milliseconds` gives them.
(struct (a) timed-result ([value : a] [start : Flonum] [end : Flonum]))

(: call-timed (All (a) (-> (-> a) (timed-result a))))
(define (call-timed thunk)
  (define start (current-inexact-milliseconds))
  (define value (thunk))
  (timed-result value start (current-inexact-milliseconds)))

;; If a step fails the subsequent steps will be `#f`.
(struct build-result ([agda-annotate : (timed-result agda-annotate-result)]
                      [typst : (U (timed-result typst-result) #f)]
                      [pagefind : (U (timed-result pagefind-result) #f)]))

(: handle-build (-> Void))
(define (handle-build)
  (define result (build (find-project-root (current-directory))))
  (print-build-result result)
  (unless (build-succeeded? result)
    (exit 1)))

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
(: build (-> Path build-result))
(define (build root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (delete-directory/files ".arb" #:must-exist? #f)
    (make-directory ".arb")
    (write-sources sources ".arb/sources.txt")
    (define agda-annotate
      (call-timed
       (thunk
         (run-agda-annotate
          (agda-annotate-input
           ".arb"
           (filter (curryr path-has-extension? #".lagda.typ") sources))))))
    (define typst
      (and (agda-annotate-result-succeeded? (timed-result-value agda-annotate))
           (call-timed
            (thunk
              (run-typst-compile
               (typst-input 'compile "site.typ" ".arb/staging" ".arb/deps"))))))
    (define pagefind
      (and typst
           (let ([dependencies
                  (typst-result-dependencies (timed-result-value typst))])
             (and dependencies (typst-dependencies-outputs dependencies)))
           (begin
             (copy-directory/files ".arb/types" ".arb/staging/types")
             (call-timed
              (thunk
                (run-pagefind (pagefind-input ".arb/staging"
                                              pagefind-root-selector
                                              pagefind-exclude-selectors)))))))
    (define result (build-result agda-annotate typst pagefind))
    (when (build-succeeded? result)
      (delete-directory/files "build" #:must-exist? #f)
      (rename-file-or-directory ".arb/staging" "build"))
    result))

(: build-succeeded? (-> build-result Boolean))
(define (build-succeeded? result)
  (define pagefind (build-result-pagefind result))
  (and pagefind (pagefind-result-succeeded? (timed-result-value pagefind))))

(: handle-watch (-> Void))
(define (handle-watch)
  (define root (find-project-root (current-directory)))
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (make-directory* ".arb")
    (write-sources sources ".arb/sources.txt")
    (display-to-file "{}" ".arb/agda-annotations.json" #:exists 'truncate)

    ;; The `--deps` Typst option causes it to write the compilation dependencies
    ;; to a file. We use this to detect compilation iterations during `typst
    ;; watch`, since parsing its terminal output (on stderr) for this
    ;; information is quite tricky. Typst writes these dependencies during
    ;; `compile_once` that is called in the body of the main watch loop, after
    ;; the outputs and diagnostics have been written (See
    ;; crates/typst-cli/src/watch.rs).
    ;;
    ;; For each compilation iteration in watch, Typst opens the path again with
    ;; `File::create` (which truncates if the file already exists), writes the
    ;; dependencies as a JSON object, and closes it. We use a FIFO to avoid the
    ;; dependencies of previous iterations getting overwritten by subsequent
    ;; ones. With a FIFO, the truncation does nothing, the writes queue up in
    ;; order, the port becomes ready when data arrives, and if we fall behind in
    ;; reading, Typst's writes block for us to catch up.
    ;;
    ;; Once the last writer of a FIFO closes, the reader gets EOF. We open the
    ;; file for writing ourselves to avoid this. We make use of the fact that
    ;; JSON objects are prefix-free, so we can delimit them without needing to
    ;; wait for EOFs.
    (define dependencies-path ".arb/deps")
    (delete-directory/files dependencies-path #:must-exist? #f)
    (make-fifo dependencies-path)
    (define dependencies-port (open-input-file dependencies-path))
    (define dependencies-writer
      (open-output-file dependencies-path #:exists 'append))
    (define-values (typst _output _input _error)
      (apply subprocess
             (current-output-port)
             (current-input-port)
             (current-error-port)
             (force typst-executable)
             (typst-arguments (typst-input 'watch
                                           "site.typ"
                                           ".arb/staging"
                                           dependencies-path))))

    ;; An interrupt from the terminal reaches Typst too, and ends the watch.
    (with-handlers ([exn:break? void])
      (dynamic-wind
       void
       (thunk
         (let loop : Void ()
           (sync (handle-evt dependencies-port
                             (λ (_)
                               (read-typst-dependencies dependencies-port)
                               (loop)))
                 (handle-evt typst
                             (λ (_)
                               (raise-user-error 'arb "typst exited"))))))
       (thunk
         (subprocess-kill typst #t)
         (close-output-port dependencies-writer)
         (close-input-port dependencies-port)
         (delete-file dependencies-path))))))

;; Report formatting

;; The step lines go to standard output. Typst's diagnostics and the output of a
;; program that failed go to standard error.
(: print-build-result (-> build-result Void))
(define (print-build-result result)
  (match-define (build-result agda-annotate typst pagefind) result)
  (print-agda-annotate-result agda-annotate)
  (when typst
    (print-typst-result typst))
  (when pagefind
    (print-pagefind-result pagefind)))

(: print-agda-annotate-result (-> (timed-result agda-annotate-result) Void))
(define (print-agda-annotate-result agda-annotate)
  (match-define (agda-annotate-result succeeded? output)
    (timed-result-value agda-annotate))
  (print-step "agda-annotate" agda-annotate (if succeeded? "" "failed"))
  (unless succeeded?
    (display output (current-error-port))))

(: print-typst-result (-> (timed-result typst-result) Void))
(define (print-typst-result typst)
  (match-define (typst-result dependencies diagnostics)
    (timed-result-value typst))
  (define outputs (and dependencies (typst-dependencies-outputs dependencies)))
  (print-step "typst"
              typst
              (if outputs (format "~a files" (length outputs)) "failed"))
  (display diagnostics (current-error-port)))

(: print-pagefind-result (-> (timed-result pagefind-result) Void))
(define (print-pagefind-result pagefind)
  (match-define (pagefind-result succeeded? output)
    (timed-result-value pagefind))
  (print-step "pagefind" pagefind (if succeeded? "" "failed"))
  (unless succeeded?
    (display output (current-error-port))))

(: print-step (-> String (timed-result Any) String Void))
(define (print-step program step summary)
  (define seconds (/ (- (timed-result-end step) (timed-result-start step))
                     1000))
  (printf "~a ~a s~a\n"
          (~a program #:min-width 14)
          (~r seconds #:precision '(= 1) #:min-width 5)
          (if (string=? summary "") "" (string-append "  " summary)))
  (flush-output))

;; `agda-annotate` wrapper

(struct agda-annotate-input ([output-directory : Path-String]
                             [sources : (Listof Path)]))

(: agda-annotate-arguments (-> agda-annotate-input (Listof Path-String)))
(define (agda-annotate-arguments input)
  (match-define (agda-annotate-input output-directory sources) input)
  (list* "-o" output-directory sources))

(struct agda-annotate-result ([succeeded? : Boolean] [output : String]))

(: run-agda-annotate (-> agda-annotate-input agda-annotate-result))
(define (run-agda-annotate input)
  ;; Agda reads its sources in the locale's encoding, and the sources are
  ;; UTF-8, so it fails where the locale is C, as in the Nix sandbox.
  (define environment
    (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! environment #"LC_ALL" #"C.UTF-8")
  ;; Forced here and not in the call, where an error would be printed to the
  ;; port that captures the output.
  (define executable (force agda-annotate-executable))
  (define output (open-output-string))
  (define succeeded?
    (parameterize ([current-environment-variables environment]
                   [current-output-port output]
                   [current-error-port output])
      (apply system* executable (agda-annotate-arguments input))))
  (agda-annotate-result succeeded? (get-output-string output)))

;; `typst` wrapper

(struct typst-input ([command : (U 'compile 'watch)]
                     [input : Path-String]
                     [output : Path-String]
                     [dependencies : Path-String]))

(: typst-arguments (-> typst-input (Listof Path-String)))
(define (typst-arguments input)
  (match-define (typst-input command input-path output-path dependencies-path)
    input)
  (append (case command
            [(compile) (list "compile")]
            [(watch) (list "watch" "--no-serve")])
          (list "--root" "."
                "--features" "bundle,html"
                "--format" "bundle"
                "--deps" dependencies-path
                "--deps-format" "json"
                input-path
                output-path)))

;; The `dependencies` field is `#f` if Typst stopped before it wrote them, for
;; example because the input file is missing. The diagnostics are the text
;; Typst printed, warnings included.
(struct typst-result ([dependencies : (U typst-dependencies #f)]
                      [diagnostics : String]))

(: run-typst-compile (-> typst-input typst-result))
(define (run-typst-compile input)
  (define executable (force typst-executable))
  (define output (open-output-string))
  (parameterize ([current-output-port output]
                 [current-error-port output])
    (apply system* executable (typst-arguments input)))
  (define dependencies-path (typst-input-dependencies input))
  (typst-result (and (file-exists? dependencies-path)
                     (call-with-input-file dependencies-path
                       read-typst-dependencies))
                (get-output-string output)))

;; Typst has a `--deps` option that writes the inputs and outputs of a compile
;; to a file. Paths under the root are relative to the current directory, and
;; paths outside it, such as files in the Typst package cache, are absolute. The
;; `outputs` field is `#f` if the compile failed. See `write_deps_json` in
;; Typst's crates/typst-cli/src/deps.rs.
(struct typst-dependencies ([inputs : (Listof Path)]
                            [outputs : (U #f (Listof Path))]))

;; Reads one JSON object. In `watch` the port is the FIFO, which we opened with
;; a write end too, so it will not ever yield EOF.
(: read-typst-dependencies (-> Input-Port typst-dependencies))
(define (read-typst-dependencies port)
  (define json (read-json port))
  (: invalid (-> Nothing))
  (define (invalid)
    (error 'arb "unexpected dependencies from Typst: ~e" json))
  (: parse-paths (-> Any (Listof Path)))
  (define (parse-paths strings)
    (if (and (list? strings) (andmap string? strings))
        (map string->path strings)
        (invalid)))
  (define table (if (hash? json) json (invalid)))
  (define outputs (hash-ref table 'outputs invalid))
  (typst-dependencies (parse-paths (hash-ref table 'inputs invalid))
                      (and (not (eq? outputs 'null))
                           (parse-paths outputs))))

;; `pagefind` wrapper

(define pagefind-root-selector ".node")
(define pagefind-exclude-selectors
  (list ".node-meta"
        ".node-rail"
        ".backmatter"
        "pre"
        ".transclusion"
        "math[display='block']"))

(struct pagefind-input ([site : Path-String]
                        [root-selector : String]
                        [exclude-selectors : (Listof String)]))

(: pagefind-arguments (-> pagefind-input (Listof Path-String)))
(define (pagefind-arguments input)
  (match-define (pagefind-input site root-selector exclude-selectors) input)
  (list "--site" site
        "--root-selector" root-selector
        "--exclude-selectors" (string-join exclude-selectors ", ")))

(struct pagefind-result ([succeeded? : Boolean] [output : String]))

(: run-pagefind (-> pagefind-input pagefind-result))
(define (run-pagefind input)
  (define executable (force pagefind-executable))
  (define output (open-output-string))
  (define succeeded?
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (apply system* executable (pagefind-arguments input))))
  (pagefind-result succeeded? (get-output-string output)))

;; Manifest

(define manifest-name "arb.toml")

(: read-manifest (-> Path (Listof String)))
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
      globs
      (raise-user-error 'arb
                        "~a must have a sources array of glob strings"
                        manifest-name)))

;; Sources

;; We collect the list of input files and pass them to Typst in a
;; newline-delimited text file.

(: source-paths (-> Path (Listof String) (Listof Path)))
(define (source-paths root globs)
  (define paths
    (for*/list : (Listof Path)
               ([glob (in-list globs)]
                [path (in-glob (build-path root glob))])
      (assert (find-relative-path root path) path?)))
  (sort (remove-duplicates paths) path<?))

(: write-sources (-> (Listof Path) Path-String Void))
(define (write-sources sources path)
  (display-lines-to-file (map path->string sources) path #:exists 'truncate))

;; Helpers

;; Walk up the directory tree looking for the first arb.toml manifest file.
(: find-project-root (-> Path Path))
(define (find-project-root directory)
  (define-values (parent _name _directory?) (split-path directory))
  (cond
    [(file-exists? (build-path directory manifest-name)) directory]
    [(path? parent) (find-project-root parent)]
    [else (raise-user-error 'arb
                            "no ~a in this directory or above it"
                            manifest-name)]))

;; Tests

(module+ test
  (require typed/rackunit)

  (: printed (-> build-result (List String String)))
  (define (printed result)
    (define output (open-output-string))
    (define error (open-output-string))
    (parameterize ([current-output-port output]
                   [current-error-port error])
      (print-build-result result))
    (list (get-output-string output) (get-output-string error)))

  ;; Examples of real program output in note builds, with Typst's warnings cut
  ;; down to the first two and the path of the checkout shortended.

  (define typst-warnings #<<END
warning: bundle export is experimental
 = hint: its behaviour may change at any time
 = hint: do not rely on this feature for production use cases

warning: overline was ignored during MathML export
   ┌─ notes/002c.typ:40:23
   │
40 │     quad f^(frak(A)_i)(overline(a)) = f^frak(A)(overline(a)),
   │                        ^^^^^^^^^^^


END
    )

  (define typst-error #<<END
error: unknown variable: nope
   ┌─ notes/002c.typ:47:1
   │
47 │ #nope
   │  ^^^^

  while including `notes/002c.typ` at site.typ:6:49
    include path
  while calling `map` at site.typ:6:2
    source-paths.map(path => (path: path, content: include path))

warning: bundle export is experimental
 = hint: its behaviour may change at any time
 = hint: do not rely on this feature for production use cases


END
    )

  (define typst-missing-input
    "error: input file not found (searched at site.typ)\n")

  (define agda-annotate-error #<<END
Checking Algebra.Definitions (/notes/subtrees/barb/source/Algebra/Definitions.lagda.typ).
/notes/subtrees/barb/source/Algebra/Definitions.lagda.typ:86.10-13: error: [NotInScope]
Not in scope:
  Set
  at /notes/subtrees/barb/source/Algebra/Definitions.lagda.typ:86.10-13
when scope checking Set

END
    )

  (define pagefind-error
    (string-append
     "[Building search indexes]\n"
     "Total: \n"
     "  Indexed 0 languages\n"
     "  Indexed 0 pages\n"
     "Error: Pagefind was not able to build an index. \n"
     "Most likely, the directory passed to Pagefind was empty or did not contain any html files.\n"))

  (define agda-annotate-succeeded
    (timed-result (agda-annotate-result #t "") 0.0 2100.0))
  (define typst-succeeded
    (timed-result (typst-result
            (typst-dependencies (list (string->path "site.typ"))
                                (for/list : (Listof Path) ([i (in-range 465)])
                                  (build-path ".arb/staging"
                                              (format "~a.html" i))))
            typst-warnings)
           2100.0
           4700.0))

  (test-case "a successful build prints three steps and Typst's warnings"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            typst-succeeded
                            (timed-result (pagefind-result #t "Finished in 0.254 seconds\n")
                                   4700.0
                                   4900.0)))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            2.6 s  465 files\n"
                          "pagefind         0.2 s\n")
           typst-warnings)))

  (test-case "a failed agda-annotate prints its output and no later step"
    (check-equal?
     (printed
      (build-result (timed-result (agda-annotate-result #f agda-annotate-error)
                                   0.0
                                   100.0)
                    #f
                    #f))
     (list "agda-annotate    0.1 s  failed\n"
           agda-annotate-error)))

  (test-case "a compile error prints Typst's diagnostics and no pagefind step"
    (check-equal?
     (printed
      (build-result agda-annotate-succeeded
                    (timed-result (typst-result (typst-dependencies null #f)
                                                typst-error)
                                  2100.0
                                   2130.0)
                    #f))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            0.0 s  failed\n")
           typst-error)))

  (test-case "Typst stopping before it writes dependencies is a failure"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            (timed-result (typst-result #f typst-missing-input)
                                   2100.0
                                   2130.0)
                            #f))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            0.0 s  failed\n")
           typst-missing-input)))

  (test-case "a failed pagefind prints its output after Typst's warnings"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            typst-succeeded
                            (timed-result (pagefind-result #f pagefind-error)
                                   4700.0
                                   4900.0)))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            2.6 s  465 files\n"
                          "pagefind         0.2 s  failed\n")
           (string-append typst-warnings pagefind-error)))))
