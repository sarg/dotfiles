(define-module (personal packages binary)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix packages))

(define-public hcloud
  (package
   (name "hcloud")
   (version "1.49.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/hetznercloud/cli/releases/download/v"
                  version "/hcloud-linux-amd64.tar.gz"))
            (sha256
             (base32 "1k3r1r0pby8agggiz87jrmr0xs0ikas3wfj7lnxg57mgdq75nvnw"))))
   (build-system binary-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments `(#:install-plan '(("hcloud" "bin/hcloud"))))
   (home-page "https://github.com/hetznercloud/cli")
   (synopsis "hcloud CLI utility")
   (description "hcloud CLI utility")
   (license license:expat)))

(define-public hugo
  (package
   (name "hugo")
   (version "0.140.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/gohugoio/hugo/releases/download/v"
                  version "/hugo_" version "_linux-amd64.tar.gz"))
            (sha256
             (base32 "0ydzhj1qxl4wyfa64f063xrrl11247p6wcky8jvhblmska21p7sm"))))
   (build-system binary-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments `(#:install-plan '(("hugo" "bin/hugo"))))
   (home-page "https://gohugo.io")
   (synopsis "hugo blogging platform")
   (description "hugo blogging platform")
   (license license:expat)))

(define-public babashka
  (package
   (name "babashka")
   (version "1.12.195")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/babashka/babashka/releases/download/v"
                  version "/babashka-" version "-linux-amd64.tar.gz"))
            (sha256
             (base32 "1g5fbr6zniy6kkawzs0ga2llhmi3hzv5b9m0wqh0hm8i21d1q9xi"))))
   (build-system binary-build-system)
   (inputs (list zlib))
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:install-plan '(("bb" "bin/bb"))
      #:patchelf-plan '(("bb" ("zlib")))))
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
