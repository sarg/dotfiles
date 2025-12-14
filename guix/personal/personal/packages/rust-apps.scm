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
  #:use-module (gnu packages llvm)
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
