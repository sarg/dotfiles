(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (personal packages emacs-xyz))

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
    (properties '((commit . "a0d6aac43fc94def29c98826e2f0088bcb703d13")))
    (version (git-version "3.0.0" "3" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/doomemacs/doomemacs")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ssdb3rlhwfrknjlxyp487r3lzmyw80dm5c1f69bfqd7ly0s2jxl"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan '(("." "share/doomemacs"))))
    (propagated-inputs (list
                        emacs-auto-minor-mode
                        emacs-gcmh
                        emacs-compat
                        emacs-nerd-icons
                        emacs-hide-mode-line
                        emacs-restart-emacs
                        emacs-better-jumper
                        emacs-smartparens
                        emacs-projectile
                        emacs-project
                        emacs-general
                        emacs-which-key))
    (home-page "https://github.com/doomemacs/doomemacs")
    (description "Doom emacs sources")
    (synopsis "Doom emacs")
    (license license:expat)))
