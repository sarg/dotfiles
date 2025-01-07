(define-module (personal services supercron)
  #:use-module (gnu services)
  #:use-module (guix build-system trivial)
  #:use-module (gnu home services shepherd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)

  #:export (supercron-root-service-type))

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

(define (supercron-shepherd-service tasks)
  (list (shepherd-service
         (documentation "Runs supercron instance.")
         (provision '(supercron))
         (start #~(let ((state-dir (string-append (getenv "XDG_STATE_HOME") "/supercron")))
                    ((@ (guix build utils) mkdir-p) state-dir)
                    (make-forkexec-constructor
                     (list
                      #$(file-append supercron "/bin/supercron")
                      "--period" "1m"
                      #$(scheme-file "tasks" #~(list #$@tasks)))
                     #:directory state-dir)))
         (stop #~(make-kill-destructor)))))

(define-public supercron-root-service-type
  (service-type
   (name 'supercron)
   (extensions
    (list (service-extension home-shepherd-service-type
                             supercron-shepherd-service)))
   (compose concatenate)
   (extend (lambda (config tasks) (append config tasks)))
   (default-value '())
   (description "Supercron service.")))
