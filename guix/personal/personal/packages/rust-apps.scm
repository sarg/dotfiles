(define-module (personal packages rust-apps)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (srfi srfi-26)
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

  #:use-module (gnu packages window-management)
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
    (properties '((commit . "6797f260ee59f4e9ad8a74dfd66f8eb49ca8e26c")))
    (version (git-version "0.1.0" "25" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/ewm")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zi52ddmfnm8wi35crq97pkddx4r485qhrhj4incmbngqs7gxa14"))))
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
                   (with-directory-excursion "lisp"
                     (substitute* "ewm.el"
                       (("\\(getenv \"EWM_MODULE_PATH\"\\)")
                        (string-append "\"" #$output "/lib/libewm_core.so\"")))

                     (for-each
                      (lambda (phase)
                        (apply (cdr phase) args))
                      (modify-phases emacs:%standard-phases
                        (delete 'unpack))))))
               
               (add-after 'build-lisp 'fix-deps
                 (lambda _
                   (chdir "compositor")
                   (delete-file "Cargo.lock")
                   (substitute* "Cargo.toml"
                     (("^elisp =.*") "elisp = { version = \"*\", features = [\"serde\"]}\n")
                     (("^elisp-emacs =.*") "elisp-emacs = { version = \"*\" }\n")
                     (("^rev =.*") "version = \"*\"\n")
                     (("^git = .*") ""))))

               (replace 'install
                 (lambda _
                   (install-file "target/release/libewm_core.so"
                                 (string-append #$output "/lib")))))))
    (native-inputs (list emacs-minimal pkg-config))
    (inputs (cons*
             dbus
             rust-elisp
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

(define-public rust-elisp
  (package
    (name "rust-elisp")
    (properties '((commit . "44f20d3ae754bbefdbeec184c5490a99935166a0")))
    (version (git-version "0.0.1" "1" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/elisp")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "187p9xq4213p09qjarhzgynpf0lf35i4c644isbd2mk8bdrc0nkg"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:cargo-package-crates ''("elisp" "elisp-emacs")))
    (inputs (my-cargo-inputs 'elisp))
    (home-page "https://codeberg.org/ezemtsov/elisp")
    (synopsis #f)
    (description #f)
    (license license:expat)))

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

(define-public podman-healthcheckd
  (package
    (name "podman-healthcheckd")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/franzos/podman-healthcheckd")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q6nrp1r0b7y3amv631l473814rcfn8f8lswf5sncshqh9ss248h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs
     (my-cargo-inputs 'podman-healthcheckd))
    (home-page "https://github.com/franzos/podman-healthcheckd")
    (synopsis "Podman healthcheck scheduler for systems without systemd")
    (description
     "Podman-healthcheckd is a daemon that schedules and runs Podman container
healthchecks on systems that do not use systemd.  It monitors running containers
and executes their configured healthcheck commands at the specified intervals.")
    (license license:expat)))
