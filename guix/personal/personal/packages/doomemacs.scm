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
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/doomemacs/doomemacs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zdg2dwxliialx87b8wkbhz57j4dqlvnpjcvmba0jyyk50dvpifa"))))
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
                        emacs-general
                        emacs-which-key))
    (home-page "https://github.com/doomemacs/doomemacs")
    (description "Doom emacs sources")
    (synopsis "Doom emacs")
    (license license:expat)))


(define-public doomemacs-module-meow
  (package
    (name "doomemacs-module-meow")
    (properties '((commit . "df4c42ae4cfc83899cf98c867ca0df633e015be9")))
    (version (git-version "0" "0" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/meow-edit/doom-meow")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15dxi3pdnvbzf4pn5pv6gfn7rawqs6pybsspjvhv1m4n0nwzk6rc"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan '(("." "share/doomemacs/modules/editor/meow"))))
    (propagated-inputs (list emacs-meow))
    (home-page "https://github.com/meow-edit/doom-meow")
    (description "Doom + Meow")
    (synopsis "Doom Meow module")
    (license license:gpl3)))
