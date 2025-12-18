(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public emacs-ob-cfg
  (package
    (name "emacs-ob-cfg")
    (version "0.1")
    (source (local-file
             (string-append (current-source-directory) "/ob-cfg.el")))
    (build-system emacs-build-system)
    (home-page #f)
    (description "helper for guix literate config")
    (synopsis description)
    (license #f)))

(define-public doomemacs
  (package
    (name "doomemacs")
    (version "20251218")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/doomemacs/doomemacs")
             (commit "05f2109345b964ff15b2ed09df55a9b67db29693")))
       (file-name (git-file-name "doomemacs" version))
       (sha256
        (base32 "00xqfi1r31ljfmippddj7ah0rlhwm32s3546cl000ff6ykdcd6ln"))))
    (build-system copy-build-system)
    (home-page "https://github.com/doomemacs/doomemacs")
    (description "Doom emacs sources")
    (synopsis "Doom emacs")
    (license license:expat)))
