(define-module (personal services doomemacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (personal packages doomemacs)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (doomemacs-config
            doomemacs-configuration
            doomemacs-service-type))

(define* (doomemacs-config #:key src inputs)
  (computed-file "doom-config"
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (guix build emacs-build-system)))

      (with-build-variables
          (map (lambda (i) (list (package-name i) (gexp-input i))) inputs)
          '("out")

        #~(begin
            (use-modules (guix build utils)
                         (guix build emacs-build-system))

            (copy-recursively #$src %output)
            (chdir %output)
            (invoke (string-append #$emacs-minimal "/bin/emacs")
                    "-L" (elpa-directory #$emacs-ob-cfg)
                    "-L" (elpa-directory #$emacs-org)
                    "-Q" "--batch" "--eval"
                    (simple-format #f "~s"
                      '(progn
                        (require 'ob-cfg)
                        (require 'ob-tangle)
                        (setq org-confirm-babel-evaluate nil)
                        (with-current-buffer
                         (find-file-noselect "config.org")
                         (org-babel-tangle nil nil "elisp")))))

            (delete-file "config.org")
            (substitute* "config.el"
              (("\\(guix/pkg '([^ )]+)" all pkg)
               (format #f "(concat ~s"
                       (or (assoc-ref %build-inputs pkg)
                           (error (format #f "~a is required for emacs config but not present in inputs" pkg)))))))))))

(define-public (doomemacs-profile emacs doom config inputs)
  (package
    (name "doomemacs-profile")
    (version "0")
    (source #f)
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f
      #:emacs emacs
      #:modules `((ice-9 ftw) (srfi srfi-26)
                  (guix build utils)
                  (guix build emacs-utils)
                  (guix build emacs-build-system))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'make-autoloads)
          (delete 'unpack)
          (delete 'build)
          (add-before 'install 'build-profile
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "DOOMLOCALDIR" ".")
              (setenv "DOOMDIR" #$config)

              (invoke "emacs" "-q" "--no-site-file" "--batch"
                      "--load" (string-append (assoc-ref inputs "doomemacs") "/early-init")
                      "--eval" (simple-format #f "~s"
                                 '(letf! (defun doom-initialize-core-packages (&optional force-p)
                                           (require 'straight)
                                           (straight--load-build-cache))

                                         (cl-delete "90-loaddefs-packages.auto.el"
                                                    doom-profile-generators
                                                    :key 'car :test 'string=)

                                         (doom-modules-initialize)
                                         (doom-profile-generate))))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((emacs (search-input-file inputs "/bin/emacs"))
                     (out (assoc-ref outputs "out")))
                (setenv "SHELL" "sh")
                (install-file
                 (string-append "etc/@/" (car (scandir "etc/@" (cut string-suffix? ".el" <>))))
                 out)
                (parameterize ((%emacs emacs))
                  (emacs-byte-compile-directory out))))))))
    (inputs (cons doom inputs))
    (native-inputs (list git))
    (description "doom profile")
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
       (lambda (config) (list (doomemacs-configuration-doomemacs config))))
      (service-extension
       home-activation-service-type
       (match-lambda
         (($ <doomemacs-configuration> emacs doomemacs config inputs)
          #~(begin
              (use-modules (guix build utils))
              (define profile-dir (string-append (getenv "HOME") "/.local/doom/etc/@"))
              (mkdir-p (dirname profile-dir))
              (when (directory-exists? profile-dir)
                (delete-file-recursively profile-dir))
              (switch-symlinks
               profile-dir
               #$(doomemacs-profile emacs doomemacs config inputs))))))
      (service-extension
       home-environment-variables-service-type
       (const '(("DOOMLOCALDIR" . "$HOME/.local/doom/"))))
      (service-extension
       home-xdg-configuration-files-service-type
       (match-lambda
         (($ <doomemacs-configuration> emacs doomemacs config inputs)
          `(("emacs" ,doomemacs)
            ("doom" ,config)))))))
    (description "Doomemacs and its config.")))

;;; Local Variables:
;;; eval: (put 'computed-file 'scheme-indent-function 1)
;;; eval: (put 'simple-format 'scheme-indent-function 2)
;;; End:
