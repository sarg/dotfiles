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

(define-public iwmenu
  (package
    (name "iwmenu")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/e-tho/iwmenu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11myjsrsl2kz8f9g6swhd5lfd3vzhkh8rsympr5vfpiiajibn1sy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (inputs (my-cargo-inputs 'iwmenu))
    (home-page "https://github.com/e-tho/iwmenu")
    (synopsis "Launcher-driven Wi-Fi manager for Linux")
    (description
     "@code{iwmenu} (iNet Wireless Menu) manages Wi-Fi through your launcher of choice.
Supported launchers are: dmenu, fuzzel, rofi, walker and custom.")
    (license license:gpl3)))

(define-public bzmenu
  (package
    (name "bzmenu")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/e-tho/bzmenu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r3l1v0kgkrfz6b7yjpsin09yy273bip3h2s7d5phrvh73ngyxp5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (cons* dbus (my-cargo-inputs 'bzmenu)))
    (home-page "https://github.com/e-tho/bzmenu")
    (synopsis "Launcher-driven Bluetooth manager for Linux")
    (description
     "@code{bzmenu} (BlueZ Menu) manages Bluetooth through your launcher of choice.
Supported launchers are: dmenu, fuzzel, rofi, walker and custom.")
    (license license:gpl3)))

(define-public pwmenu
  (package
    (name "pwmenu")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/e-tho/pwmenu")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ymg2gknv909zxvhww3q0kgjq0rfxpf5pmi06zghv8dsq0wa8ka3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs (cons* clang pipewire (my-cargo-inputs 'pwmenu)))
    (home-page "https://github.com/e-tho/pwmenu")
    (synopsis "Launcher-driven Wi-Fi manager for Linux")
    (description
     "@code{pwmenu} (PipeWire Menu) manages audio through your launcher of choice.
Supported launchers are: dmenu, fuzzel, rofi, walker and custom.")
    (license license:gpl3)))

(define-public ewm
  (package
    (name "ewm")
    (properties '((commit . "7461052b9b128faa407ef6e623424eb5c19c5e06")))
    (version (git-version "0.1.0" "2" (assoc-ref properties 'commit)))
    (source
     ;; (local-file "/storage/devel/ext/ewm" #:recursive? #t)
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/ewm")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wn0arqirsv6h9wmvlh31qryk57jkk73sh9k1aafk5zrhjmpjf3g"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-deps
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
    (native-inputs (list pkg-config))
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
             (my-cargo-inputs 'ewm)))
    (home-page "https://codeberg.org/ezemtsov/ewm")
    (synopsis "Emacs Wayland Manager")
    (description "Emacs Wayland Manager - Wayland compositor")
    (license license:gpl3+)))
