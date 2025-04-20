(define-module (personal services supercron)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system trivial)
  #:use-module (gnu home services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)

  #:export (supercron-service-type))

(define-record-type* <supercron-configuration> supercron-configuration
  make-supercron-configuration
  supercron-configuration?
  this-supercron-configuration

  (supercron   supercron-configuration-supercron          ;file-like
               (default supercron))
  (jobs        supercron-configuration-jobs           ;list of gexps
               (default '()))
  (log?        supercron-configuration-log?           ;Boolean
               (default #t))
  (period      supercron-period
               (default "1m"))
  (log-file    supercron-configuration-log-file       ;string | gexp
               (thunked)
               (default #~(string-append %user-log-dir "/supercron.log"))))

(define supercron
  (package
    (name "supercron")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/igankevich/supercron.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "03gc05xnawjajxi2bcswpzrsl2gljh280px2vh20rqa1rwcscqk4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 textual-ports)
                      (srfi srfi-26))
         (let* ((source (assoc-ref %build-inputs "source"))
                (program (string-append source "/supercron.scm"))
                (bin (string-append %output "/bin"))
                (script (string-append bin "/supercron")))
           (mkdir-p bin)

           (call-with-output-file script
             (lambda (port)
               (format port "#!~a --no-auto-compile\n!#\n~a"
                       (search-input-file %build-inputs "/bin/guile")
                       (call-with-input-file program (cut get-string-all <>)))))
           (chmod script #o555)))))
    (native-inputs (list guile-3.0))
    (home-page "https://github.com/igankevich/supercron")
    (synopsis "Persistent scheduled jobs.")
    (description "Persistence job scheduler similar to mcron.")
    (license license:gpl3+)))

(define (supercron-shepherd-service config)
  (match-record config <supercron-configuration>
                (supercron jobs log? log-file period)
    (let ((job-file (scheme-file "tasks" #~(list #$@jobs))))
      (list (shepherd-service
             (documentation "Runs supercron instance.")
             (provision '(supercron))
             (modules `(((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                        ,@%default-modules))
             (actions (list (shepherd-configuration-action job-file)))
             (start #~(let ((state-dir (string-append (getenv "XDG_STATE_HOME") "/supercron")))
                        (mkdir-p state-dir)
                        (make-forkexec-constructor
                         (list #$(file-append supercron "/bin/supercron")
                               #$@(if log? '("--verbose") '())
                               "--period" #$period
                               #$job-file)
                         #:log-file #$log-file
                         #:directory state-dir)))
             (stop #~(make-kill-destructor)))))))

(define-public supercron-service-type
  (service-type
   (name 'supercron)
   (extensions
    (list (service-extension home-shepherd-service-type
                             supercron-shepherd-service)))
   (compose concatenate)
   (extend (lambda (config jobs)
             (supercron-configuration
              (inherit config)
              (jobs (append (supercron-configuration-jobs config)
                            jobs)))))
   (default-value (supercron-configuration))
   (description "Supercron service.")))
