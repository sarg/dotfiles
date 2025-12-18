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
    (properties '((commit . "0d2f10dcabb5cee9c196b319306421c6c140cbe9")
                  (revision . "20251203")))
    (version (git-version "3.0.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/doomemacs/doomemacs")
             (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name "doomemacs" version))
       (sha256
        (base32 "04m6zavm5dqanwh960sajai0c6y537700hsvlhn7xjhraq55jbki"))))
    (build-system copy-build-system)
    (home-page "https://github.com/doomemacs/doomemacs")
    (description "Doom emacs sources")
    (synopsis "Doom emacs")
    (license license:expat)))
