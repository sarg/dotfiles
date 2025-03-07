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
   (version "1.12.196")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/babashka/babashka/releases/download/v"
                  version "/babashka-" version "-linux-amd64.tar.gz"))
            (sha256
             (base32 "13wdkd3816s8bf2rfhcqh8igaxdbr6qafhinj3lny4ycg5yg9nqq"))))
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

(define-public restic
  (let* ((version "0.17.3")
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
                 (base32 "1lyl69336y2n06zrpn7y863pyqrnlvdyydkfmrx1c4xalvnzm5sh"))))
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
