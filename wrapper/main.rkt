#lang typed/racket/base

(require racket/cmdline
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/port
         racket/promise
         racket/string
         racket/system
         typed/file/glob
         typed/json)

(require/typed racket/base
               [file-or-directory-stat
                (-> Path-String (HashTable Symbol Integer))]
               [path<? (-> Path Path Boolean)]
               [(subprocess subprocess/merged)
                (-> #f #f 'stdout Path Path-String *
                    (Values Subprocess Input-Port Output-Port #f))])
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

(define staging-directory ".arb/staging")

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
;; file that contains the Agda annotations. Typst writes into a staging
;; directory inside `.arb/`, and `publish` makes `build/` from it, so a failed
;; build leaves the previous one alone and no stale pages survive.
(: build (-> Path build-result))
(define (build root)
  (define sources (source-paths root (read-manifest root)))
  (parameterize ([current-directory root])
    (make-directory* ".arb")
    (define lock (open-output-file ".arb/lock" #:exists 'append))
    ;; We hold a lock on a `.arb/lock` file so that multiple builds or watches
    ;; do not clobber each other.
    (unless (port-try-file-lock? lock 'exclusive)
      (raise-user-error 'arb "another build or watch is running in ~a" root))
    (delete-directory/files ".arb/staging" #:must-exist? #f)
    (delete-directory/files ".arb/types" #:must-exist? #f)
    ;; A watch that was killed leaves its FIFO here, which Typst would block on.
    (delete-directory/files ".arb/deps" #:must-exist? #f)
    (write-sources sources ".arb/sources.txt")
    (define agda-annotate
      (call-timed
       (thunk
         (run-agda-annotate
          (agda-annotate-input
           ".arb"
           (agda-sources sources))))))
    (define typst
      (and (agda-annotate-result-succeeded? (timed-result-value agda-annotate))
           (call-timed
            (thunk
              (run-typst-compile
               (typst-input 'compile "site.typ" staging-directory ".arb/deps"))))))
    (define pagefind
      (and typst
           (let* ([dependencies
                   (typst-result-dependencies (timed-result-value typst))]
                  [outputs
                   (and dependencies (typst-dependencies-outputs dependencies))])
             (and outputs
                  (call-timed (thunk (publish outputs ".arb/types")))))))
    (close-output-port lock)
    (build-result agda-annotate typst pagefind)))

;; We copy the outputs out of the staging directory rather than renaming it for
;; two reasons. First, Typst does not delete old output files, so we want to
;; avoid these stale files in the build directory. Second, we want to avoid
;; subsequent watch compile iterations from conflicting with pagefind runs.
;;
;; Copying the files might race with Typst's next compile, but eh. The files
;; will be replaced with their correct version in the next iteration, and the
;; time window is small.
(: publish (-> (Listof Path) (U Path-String #f) pagefind-result))
(define (publish outputs types)
  (define publish-directory ".arb/publish")
  (delete-directory/files publish-directory #:must-exist? #f)
  (define staging (path->complete-path staging-directory))
  (for ([output (in-list outputs)])
    (define relative
      (assert (find-relative-path staging (path->complete-path output)) path?))
    (when (memq 'up (explode-path relative))
      (error 'arb "Typst wrote ~a outside ~a" output staging-directory))
    (define published (build-path publish-directory relative))
    (make-parent-directory* published)
    (copy-file output published))
  (when types
    (copy-directory/files types (build-path publish-directory "types")))
  (define result
    (run-pagefind (pagefind-input publish-directory
                                  pagefind-root-selector
                                  pagefind-exclude-selectors)))
  (when (pagefind-result-succeeded? result)
    (delete-directory/files "build" #:must-exist? #f)
    (rename-file-or-directory publish-directory "build"))
  result)

(: build-succeeded? (-> build-result Boolean))
(define (build-succeeded? result)
  (define pagefind (build-result-pagefind result))
  (and pagefind (pagefind-result-succeeded? (timed-result-value pagefind))))

(: install-agda-annotations (-> agda-annotate-result Void))
(define (install-agda-annotations result)
  (delete-directory/files ".arb/types" #:must-exist? #f)
  (cond
    [(agda-annotate-result-succeeded? result)
     (rename-file-or-directory ".arb/agda/types" ".arb/types")
     (display-to-file (file->bytes ".arb/agda/agda-annotations.json")
                      ".arb/agda-annotations.json"
                      #:exists 'truncate)]
    [else
     (display-to-file "{}" ".arb/agda-annotations.json" #:exists 'truncate)]))

;; If the compile failed the pagefind step will be `#f`.
(struct watch-result ([typst : (timed-result typst-result)]
                      [pagefind : (U (timed-result pagefind-result) #f)]))

(: handle-watch (-> Void))
(define (handle-watch)
  (define root (find-project-root (current-directory)))
  (define globs (read-manifest root))
  (define sources (source-paths root globs))
  (parameterize ([current-directory root])
    ;; If the input file is missing, Typst will print an error and wait for the
    ;; file to appear. In this case it will not print the "compiling ..." status
    ;; line our framing code below relies on, so we ensure here that the input
    ;; file exists.
    (unless (file-exists? "site.typ")
      (raise-user-error 'arb "no site.typ in ~a" root))
    (make-directory* ".arb")
    (define lock (open-output-file ".arb/lock" #:exists 'append))
    (unless (port-try-file-lock? lock 'exclusive)
      (raise-user-error 'arb "another build or watch is running in ~a" root))
    (write-sources sources ".arb/sources.txt")
    (display-to-file "{}" ".arb/agda-annotations.json" #:exists 'truncate)
    (delete-directory/files ".arb/types" #:must-exist? #f)

    (define dependencies-path ".arb/deps")
    (define custodian (make-custodian))
    ;; Forced here because an error in a thread ends only that thread.
    (define _agda-annotate-executable (force agda-annotate-executable))
    (define annotations : (Channelof (timed-result agda-annotate-result))
      (make-channel))
    (: start-agda-annotate (-> (Listof Path) Void))
    (define (start-agda-annotate sources)
      (delete-directory/files ".arb/agda" #:must-exist? #f)
      (parameterize ([current-custodian custodian]
                     [current-subprocess-custodian-mode 'kill])
        (thread
         (thunk
           (channel-put annotations
                        (call-timed
                         (thunk
                           (run-agda-annotate
                            (agda-annotate-input ".arb/agda"
                                                 (agda-sources sources)))))))))
      (void))
    (with-handlers ([exn:break? void])
      (dynamic-wind
       void
       (thunk
         (define-values (reader iterations)
           (parameterize ([current-custodian custodian]
                          [current-subprocess-custodian-mode 'kill])
             (watch-typst (typst-input 'watch
                                       "site.typ"
                                       staging-directory
                                       dependencies-path))))
         (start-agda-annotate sources)
         ;; `modified` is the modification times of the Agda sources when
         ;; agda-annotate was last started. One run happens at a time, and a
         ;; change during a run queues one more. Nothing is written for Typst
         ;; to read until its first result has arrived, since it watches no
         ;; file before that.
         (let loop : Void ([sources : (Listof Path) sources]
                           [changes : (Listof Filesystem-Change)
                                    (watch-source-directories root globs)]
                           [compiled? : Boolean #f]
                           [agda : (U 'idle 'running 'queued) 'running]
                           [modified : (Listof Integer)
                                     (for/list : (Listof Integer)
                                               ([path (agda-sources sources)])
                                       (hash-ref (file-or-directory-stat path)
                                                 'modify-time-nanoseconds))])
           (sync (handle-evt iterations
                             (λ ([typst : (timed-result typst-result)])
                               (define dependencies
                                 (typst-result-dependencies
                                  (timed-result-value typst)))
                               (define outputs
                                 (and dependencies
                                      (typst-dependencies-outputs dependencies)))
                               (define pagefind
                                 (and outputs
                                      (call-timed
                                       (thunk
                                         (publish
                                          outputs
                                          (and (directory-exists? ".arb/types")
                                               ".arb/types"))))))
                               (print-watch-result (watch-result typst pagefind))
                               (unless dependencies
                                 (raise-user-error 'arb "typst exited"))
                               (loop sources changes #t agda modified)))
                 (handle-evt (if compiled? (apply choice-evt changes) never-evt)
                             (λ (_)
                               (for-each filesystem-change-evt-cancel changes)
                               (define watched
                                 (watch-source-directories root globs))
                               (define matched (source-paths root globs))
                               (unless (equal? matched sources)
                                 (write-sources matched ".arb/sources.txt"))
                               (define times
                                 (for/list : (Listof Integer)
                                           ([path (agda-sources matched)])
                                   (hash-ref (file-or-directory-stat path)
                                             'modify-time-nanoseconds)))
                               (cond
                                 [(equal? times modified)
                                  (loop matched watched #t agda modified)]
                                 [(eq? agda 'idle)
                                  (start-agda-annotate matched)
                                  (loop matched watched #t 'running times)]
                                 [else
                                  (loop matched watched #t 'queued times)])))
                 (handle-evt (if compiled? annotations never-evt)
                             (λ ([agda-annotate
                                  : (timed-result agda-annotate-result)])
                               (install-agda-annotations
                                (timed-result-value agda-annotate))
                               (print-agda-annotate-result agda-annotate)
                               (cond
                                 [(eq? agda 'queued)
                                  (start-agda-annotate sources)
                                  (loop sources changes #t 'running modified)]
                                 [else
                                  (loop sources changes #t 'idle modified)])))
                 ;; An error in a thread ends only that thread.
                 (handle-evt reader
                             (λ (_)
                               (raise-user-error
                                'arb
                                "stopped reading Typst's output"))))))
       (thunk
         (custodian-shutdown-all custodian)
         (delete-directory/files dependencies-path #:must-exist? #f)
         (close-output-port lock))))))

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

(: print-watch-result (-> watch-result Void))
(define (print-watch-result result)
  (match-define (watch-result typst pagefind) result)
  (print-typst-result typst)
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
  (for ([diagnostic (in-list diagnostics)]
        #:unless (ignored-typst-warning? diagnostic))
    (displayln (typst-diagnostic->string diagnostic) (current-error-port))))

(define ignored-typst-warnings
  (list "bundle export is experimental"
        "overline was ignored during MathML export"))

(: ignored-typst-warning? (-> typst-diagnostic Boolean))
(define (ignored-typst-warning? diagnostic)
  (and (not (typst-error? diagnostic))
       (member (typst-diagnostic-message diagnostic) ignored-typst-warnings)
       #t))

(: typst-diagnostic->string (-> typst-diagnostic String))
(define (typst-diagnostic->string diagnostic)
  (match-define (typst-diagnostic severity location message) diagnostic)
  (format "~a~a: ~a"
          (match location
            [(typst-location package path line column)
             (format "~a~a:~a:~a: "
                     (if package (string-append package "/") "")
                     path
                     line
                     column)]
            [#f ""])
          severity
          message))

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
                "--diagnostic-format" "short"
                input-path
                output-path)))

;; The `dependencies` field is `#f` if Typst stopped before it wrote them, for
;; example because the input file is missing.
(struct typst-result ([dependencies : (U typst-dependencies #f)]
                      [diagnostics : (Listof typst-diagnostic)]))

(: run-typst-compile (-> typst-input typst-result))
(define (run-typst-compile input)
  (define executable (force typst-executable))
  (define output (open-output-string))
  (define succeeded?
    (parameterize ([current-output-port output]
                   [current-error-port output])
      (apply system* executable (typst-arguments input))))
  (define diagnostics (parse-typst-diagnostics (get-output-string output)))
  (unless (eq? succeeded? (not (ormap typst-error? diagnostics)))
    (error 'arb
           "Typst's exit code and its diagnostics disagree: ~e"
           (get-output-string output)))
  (define dependencies-path (typst-input-dependencies input))
  (typst-result (and (file-exists? dependencies-path)
                     (call-with-input-file dependencies-path
                       read-typst-dependencies))
                diagnostics))

(struct typst-location ([package : (U String #f)]
                        [path : String]
                        [line : Natural]
                        [column : Natural])
  #:transparent)
(struct typst-diagnostic ([severity : (U 'error 'warning)]
                          [location : (U typst-location #f)]
                          [message : String])
  #:transparent)

(: typst-error? (-> typst-diagnostic Boolean))
(define (typst-error? diagnostic)
  (eq? (typst-diagnostic-severity diagnostic) 'error))

;; Regex for the first line of a diagnostic in Typst's `short` format, which
;; consists of an optional location, a severity, and a message. Examples:
;;
;;   notes/002c.typ:47:1: error: unknown variable: nope
;;
;;   @local/arb:0.1.0/template.typ:2:2: error: assertion failed
;;
;;   warning: bundle export is experimental
(define typst-diagnostic-pattern
  (let* ([natural "[0-9]+"]
         [identifier "[a-zA-Z_][a-zA-Z0-9_-]*"]
         [package (string-append "@" identifier "/" identifier
                                 ":" natural "[.]" natural "[.]" natural)]
         [location (string-append "(?:(" package ")/)?"
                                  "([^:]+)"
                                  ":(" natural ")"
                                  ":(" natural ")")])
    (pregexp (string-append "^"
                            "(?:" location ": )?"
                            "(error|warning): "
                            "(.*)"
                            "$"))))

(: parse-typst-diagnostics (-> String (Listof typst-diagnostic)))
(define (parse-typst-diagnostics output)
  (: unexpected (-> Nothing))
  (define (unexpected)
    (error 'arb "unexpected diagnostics from Typst: ~e" output))
  (: parse-natural (-> (U String #f) Natural))
  (define (parse-natural digits)
    (assert (string->number (or digits (unexpected)))
            exact-nonnegative-integer?))
  (unless (or (string=? output "") (string-suffix? output "\n"))
    (unexpected))
  (define diagnostics
    (for/fold ([diagnostics : (Listof typst-diagnostic) null]
               #:result (reverse diagnostics))
              ([line (in-list (port->lines (open-input-string output)))])
      (match (regexp-match typst-diagnostic-pattern line)
        [(list _
               package
               path
               line-number
               column
               (? string? severity)
               (? string? message))
         (cons (typst-diagnostic
                (if (string=? severity "error") 'error 'warning)
                (and path
                     (typst-location package
                                     path
                                     (parse-natural line-number)
                                     (parse-natural column)))
                message)
               diagnostics)]
        [_
         (match diagnostics
           [(cons (typst-diagnostic severity location message) earlier)
            (cons (typst-diagnostic severity
                                    location
                                    (string-append message "\n" line))
                  earlier)]
           [_ (unexpected)])])))
  (when (ormap typst-error? (dropf diagnostics typst-error?))
    (unexpected))
  diagnostics)

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

;; The `--deps` Typst option causes it to write the compilation dependencies
;; to a file. We use this to detect compilation iterations during `typst
;; watch`. Typst writes these dependencies during `compile_once` that is called
;; in the body of the main watch loop, after the outputs and diagnostics have
;; been written (See crates/typst-cli/src/watch.rs).
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
(: watch-typst (-> typst-input
                   (Values Thread (Channelof (timed-result typst-result)))))
(define (watch-typst input)
  (define dependencies-path (typst-input-dependencies input))
  (delete-directory/files dependencies-path #:must-exist? #f)
  (make-fifo dependencies-path)
  (define dependencies-port (open-input-file dependencies-path))
  (define _dependencies-writer
    (open-output-file dependencies-path #:exists 'append))
  (define-values (_typst typst-output typst-standard-input _error)
    (apply subprocess/merged
           #f
           #f
           'stdout
           (force typst-executable)
           (typst-arguments input)))
  (close-output-port typst-standard-input)

  (define iterations : (Channelof (timed-result typst-result)) (make-channel))
  (define output (open-output-bytes))
  ;; Our choice of size here is arbitrary. The `read-output` reads in chunks of
  ;; `chunk-size` until the pipe is empty.
  (define chunk-size (make-bytes 4096))
  ;; The boolean is "is the pipe still open?""
  (: read-output (-> Boolean))
  (define (read-output)
    (define count (read-bytes-avail!* chunk-size typst-output))
    (cond
      [(eof-object? count) #f]
      [(exact-positive-integer? count)
       (write-bytes chunk-size output 0 count)
       (read-output)]
      [else #t]))

  ;; `start` is the time at which the first byte in `output` arrived, or `#f`
  ;; while `output` is empty.
  (define reader
    (thread
     (thunk
       (let loop : Void ([start : (U Flonum #f) #f])
         (sync
          (handle-evt
           typst-output
           (λ (_)
             (define now (current-inexact-milliseconds))
             (if (read-output)
                 (loop (or start now))
                 (channel-put
                  iterations
                  (timed-result
                   (typst-result #f
                                 (parse-typst-diagnostics
                                  (bytes->string/utf-8 (get-output-bytes output)
                                                       #\uFFFD)))
                   (or start now)
                   now)))))
          (handle-evt
           dependencies-port
           (λ (_)
             (define dependencies (read-typst-dependencies dependencies-port))
             (define end (current-inexact-milliseconds))
             (read-output)
             (define read (get-output-bytes output #t))
             (define-values (succeeded? diagnostics rest)
               (parse-typst-watch-iteration read))
             (unless (eq? succeeded?
                          (and (typst-dependencies-outputs dependencies) #t))
               (error 'arb
                      "Typst's status and its dependencies disagree: ~e"
                      read))
             (write-bytes rest output)
             (channel-put iterations
                          (timed-result (typst-result dependencies diagnostics)
                                        (or start end)
                                        end))
             (loop (and (not (bytes=? rest #"")) end)))))))))
  (values reader iterations))

;; In watch mode Typst prints a status when an iteration begins and another
;; when the compile has ended, each in this form, and then the diagnostics. See
;; `Status::print` in Typst's crates/typst-cli/src/watch.rs.
;;
;;   watching site.typ
;;   writing to .arb/staging
;;
;;   [12:00:00] compiled with warnings in 2.61 s
;;
(define typst-watch-status-pattern
  (byte-pregexp
   (bytes-append #"^watching [^\n]*\n"
                 #"writing to [^\n]*\n"
                 #"\n"
                 #"\\[\\d\\d:\\d\\d:\\d\\d\\] ([^\n]*)\n"
                 #"\n")))

;; Takes the oldest iteration off the front of what `typst watch` has printed:
;; whether its compile succeeded, its diagnostics, and the output after it, which
;; belongs to the next iteration. The output must begin with a "compiling ..."
;; status and hold the whole iteration, which it does once the iteration's
;; dependencies have arrived. The iteration ends where the next status begins.
;; Typst prints a status in several writes, of which the first is "watching", so
;; the output can end partway through one. The result status must agree with
;; the diagnostics.
(: parse-typst-watch-iteration
   (-> Bytes (Values Boolean (Listof typst-diagnostic) Bytes)))
(define (parse-typst-watch-iteration output)
  (: unexpected (-> Nothing))
  (define (unexpected)
    (error 'arb "unexpected output from typst watch: ~e" output))
  (: parse-status (-> Bytes (Values Bytes Bytes)))
  (define (parse-status text)
    (match (regexp-match-positions typst-watch-status-pattern text)
      [(list (cons _ end) (cons message-start message-end))
       (values (subbytes text message-start message-end) (subbytes text end))]
      [_ (unexpected)]))
  (define-values (compiling after-compiling) (parse-status output))
  (unless (bytes=? compiling #"compiling ...")
    (unexpected))
  (define-values (result after-result) (parse-status after-compiling))
  (define end
    (match (regexp-match-positions #rx#"(?m:^watching)" after-result)
      [(list (cons start _)) start]
      [_ (bytes-length after-result)]))
  (define diagnostics
    (parse-typst-diagnostics
     (bytes->string/utf-8 (subbytes after-result 0 end) #\uFFFD)))
  (define errors? (ormap typst-error? diagnostics))
  (define succeeded?
    (cond
      [(regexp-match? #rx#"^compiled successfully in " result)
       (unless (null? diagnostics)
         (unexpected))
       #t]
      [(regexp-match? #rx#"^compiled with warnings in " result)
       (when (or (null? diagnostics) errors?)
         (unexpected))
       #t]
      [(bytes=? result #"compiled with errors")
       (unless errors?
         (unexpected))
       #f]
      [else (unexpected)]))
  (values succeeded? diagnostics (subbytes after-result end)))

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

(: agda-sources (-> (Listof Path) (Listof Path)))
(define (agda-sources sources)
  (filter (curryr path-has-extension? #".lagda.typ") sources))

(define-type Filesystem-Change (Rec x (Evtof x)))

(: watch-source-directories (-> Path (Listof String) (Listof Filesystem-Change)))
(define (watch-source-directories root globs)
  (define bases
    (remove-duplicates
     (map (λ ([glob : String]) (glob-directory root glob)) globs)))
  (for*/list : (Listof Filesystem-Change)
             ([base (in-list bases)]
              #:when (directory-exists? base)
              [directory (in-list (find-files directory-exists? base))]
              [change (in-value
                       (with-handlers ([exn:fail:filesystem? (const #f)])
                         (filesystem-change-evt directory)))]
              #:when change)
    change))

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

;; A value together with the times at which computing it started and ended, in
;; milliseconds as `current-inexact-milliseconds` gives them.
(struct (a) timed-result ([value : a] [start : Flonum] [end : Flonum]))

(: call-timed (All (a) (-> (-> a) (timed-result a))))
(define (call-timed thunk)
  (define start (current-inexact-milliseconds))
  (define value (thunk))
  (timed-result value start (current-inexact-milliseconds)))

(: glob-directory (-> Path String Path))
(define (glob-directory root glob)
  (apply build-path
         root
         (takef (string-split glob "/")
                (λ ([name : String])
                  (not (regexp-match? #rx"[*?{[]" name))))))

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

  ;; Examples of real program output, with Typst's warnings cut down and the
  ;; path of the checkout shortened.

  (define typst-warnings-output
    (string-append
     "warning: bundle export is experimental\n"
     "notes/002c.typ:40:23: warning: overline was ignored during MathML export\n"
     "notes/002c.typ:40:48: warning: overline was ignored during MathML export\n"))

  (define typst-error-output
    (string-append
     "notes/002c.typ:47:1: error: unknown variable: nope\n"
     "warning: bundle export is experimental\n"))

  (define typst-package-error-output
    (string-append
     "@local/demo:0.1.0/lib.typ:5:12: error: panicked with: (\n"
     "  alpha: \"a\",\n"
     "  gamma: (1, 2, 3),\n"
     ")\n"
     "main.typ:1:16: warning: unknown font family: no such font\n"))

  (define typst-missing-input-output
    "error: input file not found (searched at site.typ)\n")

  (: overline-warning (-> Natural typst-diagnostic))
  (define (overline-warning column)
    (typst-diagnostic 'warning
                      (typst-location #f "notes/002c.typ" 40 column)
                      "overline was ignored during MathML export"))
  (define bundle-warning
    (typst-diagnostic 'warning #f "bundle export is experimental"))
  (define typst-warnings
    (list bundle-warning (overline-warning 23) (overline-warning 48)))
  (define typst-error
    (list (typst-diagnostic 'error
                            (typst-location #f "notes/002c.typ" 47 1)
                            "unknown variable: nope")
          bundle-warning))
  (define typst-package-error
    (list (typst-diagnostic
           'error
           (typst-location "@local/demo:0.1.0" "lib.typ" 5 12)
           "panicked with: (\n  alpha: \"a\",\n  gamma: (1, 2, 3),\n)")
          (typst-diagnostic 'warning
                            (typst-location #f "main.typ" 1 16)
                            "unknown font family: no such font")))
  (define typst-missing-input
    (list (typst-diagnostic 'error
                            #f
                            "input file not found (searched at site.typ)")))

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
                                       (for/list : (Listof Path)
                                                 ([i (in-range 465)])
                                         (build-path ".arb/staging"
                                                     (format "~a.html" i))))
                   typst-warnings)
                  2100.0
                  4700.0))

  (test-case "a successful build prints three steps and no ignored warning"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            typst-succeeded
                            (timed-result
                             (pagefind-result #t "Finished in 0.254 seconds\n")
                             4700.0
                             4900.0)))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            2.6 s  465 files\n"
                          "pagefind         0.2 s\n")
           "")))

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

  (test-case "a compile error prints Typst's error and no pagefind step"
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
           "notes/002c.typ:47:1: error: unknown variable: nope\n")))

  (test-case "diagnostics are printed as Typst prints them"
    (check-equal?
     (printed
      (build-result agda-annotate-succeeded
                    (timed-result (typst-result (typst-dependencies null #f)
                                                typst-package-error)
                                  2100.0
                                  2130.0)
                    #f))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            0.0 s  failed\n")
           typst-package-error-output)))

  (test-case "Typst stopping before it writes dependencies is a failure"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            (timed-result (typst-result #f typst-missing-input)
                                          2100.0
                                          2130.0)
                            #f))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            0.0 s  failed\n")
           typst-missing-input-output)))

  (test-case "a failed pagefind prints its output"
    (check-equal?
     (printed (build-result agda-annotate-succeeded
                            typst-succeeded
                            (timed-result (pagefind-result #f pagefind-error)
                                          4700.0
                                          4900.0)))
     (list (string-append "agda-annotate    2.1 s\n"
                          "typst            2.6 s  465 files\n"
                          "pagefind         0.2 s  failed\n")
           pagefind-error)))

  (test-case "diagnostics have an optional location, a severity and a message"
    (check-equal? (parse-typst-diagnostics "") null)
    (check-equal? (parse-typst-diagnostics typst-warnings-output)
                  typst-warnings)
    (check-equal? (parse-typst-diagnostics typst-error-output) typst-error)
    (check-equal? (parse-typst-diagnostics typst-missing-input-output)
                  typst-missing-input))

  (test-case "a package's path has a colon, and a message can span lines"
    (check-equal? (parse-typst-diagnostics typst-package-error-output)
                  typst-package-error))

  (test-case "diagnostics of any other shape are an error"
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics "  alpha: \"a\",\n")))
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics "note: something else\n")))
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics
                       "@local/demo:0.1/lib.typ:5:12: error: no patch\n")))
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics
                       "a directory: with a colon/a.typ:1:1: error: nope\n")))
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics
                       "warning: bundle export is experimental")))
    (check-exn exn:fail?
               (thunk (parse-typst-diagnostics
                       (string-append typst-warnings-output
                                      typst-error-output)))))

  (: parsed (-> Bytes (List Boolean (Listof typst-diagnostic) Bytes)))
  (define (parsed output)
    (define-values (succeeded? diagnostics rest)
      (parse-typst-watch-iteration output))
    (list succeeded? diagnostics rest))

  (: status (-> Bytes Bytes))
  (define (status message)
    (bytes-append #"watching site.typ\n"
                  #"writing to .arb/staging\n"
                  #"\n"
                  #"[12:00:00] " message #"\n"
                  #"\n"))

  (define compiling (status #"compiling ..."))
  (define successfully (status #"compiled successfully in 310.5 ms"))
  (define with-warnings
    (bytes-append compiling
                  (status #"compiled with warnings in 2.61 s")
                  (string->bytes/utf-8 typst-warnings-output)))
  (define with-errors
    (bytes-append compiling
                  (status #"compiled with errors")
                  (string->bytes/utf-8 typst-error-output)))

  (test-case "an iteration's diagnostics are what follows its two statuses"
    (check-equal? (parsed with-warnings) (list #t typst-warnings #""))
    (check-equal? (parsed with-errors) (list #f typst-error #""))
    (check-equal? (parsed (bytes-append compiling successfully))
                  (list #t null #"")))

  (test-case "the next iteration's output is left over, however much has arrived"
    (check-equal? (parsed (bytes-append with-warnings #"watching"))
                  (list #t typst-warnings #"watching"))
    (check-equal? (parsed (bytes-append with-warnings compiling))
                  (list #t typst-warnings compiling))
    (check-equal? (parsed (bytes-append with-warnings with-errors))
                  (list #t typst-warnings with-errors)))

  (test-case "output that does not begin with a compiling status is an error"
    (check-exn exn:fail? (thunk (parsed #"")))
    (check-exn exn:fail? (thunk (parsed compiling)))
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               (status #"compiled with errors")
                               (string->bytes/utf-8 typst-error-output)))))
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               compiling
                               (status #"something else"))))))

  (test-case "a result status that disagrees with the diagnostics is an error"
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               compiling
                               successfully
                               (string->bytes/utf-8 typst-warnings-output)))))
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               compiling
                               (status #"compiled with warnings in 2.61 s")))))
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               compiling
                               (status #"compiled with warnings in 2.61 s")
                               (string->bytes/utf-8 typst-error-output)))))
    (check-exn exn:fail?
               (thunk (parsed (bytes-append
                               compiling
                               (status #"compiled with errors")
                               (string->bytes/utf-8 typst-warnings-output)))))))
