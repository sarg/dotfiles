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
    (properties '((commit . "3e15fb36d7f94f0a218bda977be4d3f5da983a71")))
    (version (git-version "3.0.0" "2" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/doomemacs/doomemacs")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name "doomemacs" version))
       (sha256
        (base32 "0fd5vma846cjby87ysm33fdkfnnp73xnyj931x0xq554zpvfvgs0"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules `(((guix build emacs-build-system) #:prefix emacs:)
                  (guix build copy-build-system)
                  (guix build utils))
      #:imported-modules `(,@%emacs-build-system-modules
                           ,@%copy-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-straight
            (lambda _
              (substitute* "lisp/lib/packages.el"
                (("\\(doom-path straight-base-dir \".*\"\\)")
                 (format #f "~s" (emacs:elpa-directory #$(this-package-input "emacs-straight"))))

                (("\\(doom--ensure-core-packages")
                 "(ignore"))
              (for-each
               (lambda (name)
                 (substitute* name
                   (("\\(package! [^ )]+" a)
                    (string-append a " :built-in 'prefer"))))
               (find-files "." "packages.el$")))))))
    (propagated-inputs (list
                        emacs-straight
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
