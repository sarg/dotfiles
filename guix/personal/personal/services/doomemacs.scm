(define-module (personal services doomemacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (personal packages doomemacs)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:export (doomemacs-config
            doomemacs-configuration
            doomemacs-service-type))

(define* (doomemacs-config #:key src inputs)
  (package
    (name "doomemacs-config")
    (version "0")
    (source src)
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'make-autoloads)
          (delete 'build)
          (add-before 'install 'tangle
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (search-input-file inputs "/bin/emacs")
                      "-Q" "--batch" "--eval"
                      (simple-format #f "~s"
                                     '(progn
                                       (require 'ob-cfg)
                                       (require 'ob-tangle)
                                       (setq org-confirm-babel-evaluate nil)
                                       (with-current-buffer
                                        (find-file-noselect "config.org")
                                        (org-babel-tangle nil nil "elisp")))))))
          (add-after 'tangle 'patch-guix-pkgs
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "config.el"
                (("\\(guix/pkg '([^ )]+)" _ pkg)
                 (format #f "(concat ~s"
                         (or (assoc-ref inputs pkg)
                             (error (format #f "~a is required for emacs config but not present in inputs" pkg))))))))
          (replace 'install
            (lambda _
              (delete-file "config.org")
              (copy-recursively "." #$output))))))
    (inputs inputs)
    (native-inputs (list emacs-org emacs-ob-cfg emacs-next-minimal))
    (description "doom config")
    (home-page #f)
    (synopsis #f)
    (license #f)))

(define-configuration/no-serialization doomemacs-configuration
  (emacs
   file-like
   "Emacs package to use.")
  (doomemacs
   file-like
   "Doomemacs package to use.")
  (config
   file-like
   "Doom config package.")
  (inputs
   (list-of-packages '())
   "Additional packages to install."))

(define doomemacs-service-type
  (service-type
   (name 'doomemacs)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      (lambda (config)
        (append
         (list (doomemacs-configuration-emacs config))
         (map cadr
              (package-propagated-inputs
               (doomemacs-configuration-doomemacs config)))
         (doomemacs-configuration-inputs config))))
     (service-extension
      home-activation-service-type
      (lambda (config)
        #~(begin
            (use-modules (guix build utils))
            (invoke (string-append
                     #$(doomemacs-configuration-doomemacs config)
                     "/bin/doom") "sync"))))
     (service-extension
      home-environment-variables-service-type
      (const '(("DOOMLOCALDIR" . "$HOME/.local/doom/"))))
     (service-extension
      home-xdg-configuration-files-service-type
      (lambda (config)
        `(("emacs" ,(doomemacs-configuration-doomemacs config))
          ("doom" ,(doomemacs-configuration-config config)))))))
   (description "Doomemacs and its config.")))
