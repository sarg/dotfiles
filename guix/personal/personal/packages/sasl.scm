(define-module (personal packages sasl)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public cyrus-sasl-xoauth2
  (package
    (name "cyrus-sasl-xoauth2")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1py9f1mn5k5xihrk0lfrwr6723c22gjb7lmgya83ibvislm2x3wl"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list (string-append "--with-cyrus-sasl="
                                                    #$output)
                                     "--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'bootstrap 'fix-autogen
                          (lambda _
                            ;; autogen.sh is executable but does not have a shebang.
                            (chmod "autogen.sh" #o400))))))
    (inputs (list cyrus-sasl))
    (native-inputs (list autoconf automake libtool))
    (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
    (synopsis "XOAUTH2 plugin for Cyrus SASL")
    (description "Adds support for XOAUTH2 authentication to Cyrus SASL.  This
package can be used with isync to fetch mail from servers that support it.")
    (license license:expat)))
