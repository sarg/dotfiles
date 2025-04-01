(define-module (personal packages binary)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages java)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix packages))

(define-public babashka
  (package
   (name "babashka")
   (version "1.12.197")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/babashka/babashka/releases/download/v"
                  version "/babashka-" version "-linux-amd64.tar.gz"))
            (sha256
             (base32 "0p3025pnf58218ngjdz9kcpr43lyh1c0dljs5ahgyjmpm62lyh44"))))
   (build-system binary-build-system)
   (inputs (list zlib openjdk))
   (supported-systems '("x86_64-linux"))
   (arguments
    (list
     #:install-plan #~'(("bb" "bin/bb"))
     #:patchelf-plan #~'(("bb" ("zlib")))

     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda _
             (wrap-program (string-append #$output "/bin/bb")
               `("JAVA_HOME" = (#$openjdk))))))))
   (home-page "https://babashka.org")
   (synopsis "babashka clojure scripting runtime")
   (description "babashka clojure scripting runtime")
   (license license:epl1.0)))

(define-public oama
  (package
    (name "oama")
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pdobsan/oama/releases/download/"
                    version "/oama-" version "-Linux-x86_64.tar.gz"))
              (sha256
               (base32 "1pcazb8zmjs6nhhl49wdvv85qnw03b1p0f0vhs6mwn5si705jf69"))))

    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments (list
                #:install-plan
                #~(list '("oama" "bin/oama"))))
    (home-page "https://github.com/pdobsan/oama")
    (synopsis "OAuth credential MAnager")
    (description "Provide OAuth2 capabilities to IMAP/SMTP clients.")
    (license license:bsd-3)))

(define-public restic
  (let* ((version "0.18.0")
         (pkg (string-append "restic_" version "_linux_amd64")))
    (package
      (name "restic")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/restic/restic/releases/download/v"
                      version "/" pkg ".bz2"))
                (sha256
                 (base32 "13j3fhip4fss4y8fxk360rl2mk4nw5cap3gx9g85i45myn5xvxlq"))))
      (build-system binary-build-system)
      (supported-systems '("x86_64-linux"))
      (arguments (list
                  #:install-plan
                  #~(list '(#$pkg "bin/restic"))
                  #:phases
                  #~(modify-phases %standard-phases
                      (add-before 'install 'chmod
                        (lambda _
                          (chmod #$pkg #o555))))))
      (home-page "https://restic.net")
      (synopsis "backup software")
      (description "backup software")
      (license license:bsd-2))))
