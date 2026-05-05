(define-module (personal packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages build-tools)

  #:use-module (gnu packages wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config))

(define-syntax-rule (my-cargo-inputs name)
  (cargo-inputs name #:module '(personal packages rust-crates)))

(define-public emacs-ewm
  (package
    (name "emacs-ewm")
    (properties '((commit . "6a954996596b6759c18d1f0549bf447d256b9e75")))
    (version (git-version "0.1.0" "11" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/ewm")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18glgjfmn9mg9diqag5g50xr8g0dfd15h7jyjppa0ahpbic46g1v"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:modules '((guix build cargo-build-system)
                       ((guix build emacs-build-system) #:prefix emacs:)
                       (guix build emacs-utils)
                       (guix build utils))
           #:imported-modules `(,@%cargo-build-system-modules
                                (guix build emacs-utils)
                                (guix build emacs-build-system))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-lisp
                 (lambda args
                   (chdir "lisp")

                   (substitute* "ewm.el"
                     (("\\(getenv \"EWM_MODULE_PATH\"\\)")
                      (string-append "\"" #$output "/lib/libewm_core.so\"")))

                   (for-each
                    (lambda (phase)
                      (apply (cdr phase) args))
                    (modify-phases emacs:%standard-phases
                      (delete 'unpack)))
                   
                   (chdir "..")))

               (add-after 'build-lisp 'fix-deps
                 (lambda _
                   (chdir "compositor")
                   (delete-file "Cargo.lock")
                   (substitute* "Cargo.toml"
                     (("^rev =.*") "version = \"*\"\n")
                     (("^git = .*") ""))))

               (replace 'install
                 (lambda _
                   (install-file "target/release/libewm_core.so"
                                 (string-append #$output "/lib")))))))
    (native-inputs (list emacs-minimal pkg-config))
    (inputs (cons*
             dbus
             libdisplay-info
             libinput-minimal
             libseat
             libxkbcommon
             mesa
             pipewire
             wayland
             glib
             libx11
             libxcursor
             libxrandr
             libxi
             libdrm
             (my-cargo-inputs 'emacs-ewm)))
    (home-page "https://codeberg.org/ezemtsov/ewm")
    (synopsis "Emacs Wayland Manager")
    (description "Emacs Wayland Manager - Wayland compositor")
    (license license:gpl3+)))

(define-public emacs-reka
  (package
    (name "emacs-reka")
    (properties '((commit . "a8730c31489f3ac30a428249a67233ae64515064")))
    (version (git-version "0.1.0" "2" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/tazjin/reka")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10na58mrp9w6mpzkwzsrbz4z1zqmzlyxk8irsx1ix2xsp5gbvbfl"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:modules '((guix build cargo-build-system)
                       ((guix build emacs-build-system) #:prefix emacs:)
                       (guix build emacs-utils)
                       (guix build utils))
           #:imported-modules `(,@%cargo-build-system-modules
                                (guix build emacs-utils)
                                (guix build emacs-build-system))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-lisp
                 (lambda args
                   (chdir "lisp")

                   (substitute* "reka.el"
                     (("require 'libreka")
                      (string-append "module-load \"" #$output "/lib/libreka.so\"")))

                   (for-each
                    (lambda (phase)
                      (apply (cdr phase) args))
                    (modify-phases emacs:%standard-phases
                      (delete 'unpack)))
                   
                   (chdir "..")))
               
               (replace 'install
                 (lambda _
                   (install-file "target/release/libreka.so"
                                 (string-append #$output "/lib")))))))
    (native-inputs (list emacs-minimal pkg-config))
    (inputs (cons*
             wayland
             libxkbcommon
             (my-cargo-inputs 'emacs-reka)))
    (home-page "https://codeberg.org/tazjin/reka")
    (synopsis "Emacs Wayland Manager")
    (description "Emacs Wayland Manager - Wayland compositor")
    (license license:gpl3+)))
