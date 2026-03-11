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

(define-public rust-emacs-0.20.0.34aaff9
  (let ((commit "34aaff90d22d8f5f876023ab779fe12dfffdd518"))
    (hidden-package
     (package
       (name "rust-emacs")
       (version (git-version "0.20.0" "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ubolonton/emacs-module-rs")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1f9rmx911ydqnvsjmm5f7y6amfaq6rr525a22a1a03w407jjyjd3"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("emacs-macros" "emacs_module" "emacs")))
       (inputs (my-cargo-inputs 'rust-emacs))
       (home-page #f)
       (synopsis #f)
       (description #f)
       (license #f)))))

(define-public emacs-ewm
  (package
    (name "emacs-ewm")
    (properties '((commit . "ce39d62123d062394682bb8fe3ae3682c9798f82")))
    (version (git-version "0.1.0" "4" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/ewm")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02ssfpbm6lvmjw2d64ljhl9wxjdjl8yvgadf9g83l01lcckcw3qh"))))
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
             rust-emacs-0.20.0.34aaff9
             (my-cargo-inputs 'emacs-ewm)))
    (home-page "https://codeberg.org/ezemtsov/ewm")
    (synopsis "Emacs Wayland Manager")
    (description "Emacs Wayland Manager - Wayland compositor")
    (license license:gpl3+)))
