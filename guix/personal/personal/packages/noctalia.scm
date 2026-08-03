;; https://github.com/noctalia-dev/noctalia-shell/blob/main/noctalia.scm#L1
(define-module (personal packages noctalia)
  ;; Utilities
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  ;; Guix origin methods
  #:use-module (guix git-download)
  ;; Guix build systems
  #:use-module (guix build-system meson)
  ;; Guix packages
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define wayland-protocols-1.48
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.48")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zqnn7bwqzifchjhclrrcqnp39cpd3nnf6nbd9bav2hwhcx92mwy"))))))

(define-public noctalia-shell
  (package
    (name "noctalia-shell")
    (version "5.0.0-beta.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/noctalia-dev/noctalia-shell")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06knh7g9vzp65dz80x7pb9rwgih6hh0vyxi0ymk5a35nihs4j6gm"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-for-build
                 (lambda _
                   ;; /bin/sh doesn't exist in the build environment.
                   (substitute* "tests/process_test.cpp"
                     (("/bin/(sh)" _ cmd)
                      (which cmd)))
                   ;; Adjust import paths for STB headers packaged in Guix.
                   (substitute* (find-files "." "\\.cpp$|^meson\\.build$")
                     (("\\bstb/stb_") "stb_")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           curl
           fontconfig
           freetype
           glib
           gmp
           harfbuzz
           jemalloc
           mpfr
           (librsvg-for-system)
           libqalculate
           libical
           libjxl
           libwebp
           libsndfile
           libxkbcommon
           libxml2
           libsecret
           libsodium
           linux-pam
           md4c
           mesa
           nlohmann-json
           pango
           pipewire
           polkit
           sdbus-c++
           stb-image-resize2
           stb-image-write
           tomlplusplus
           wayland
           wayland-protocols-1.48
           wireplumber))
    (home-page "https://noctalia.dev/")
    (synopsis "Wayland shell and bar")
    (description
     "Noctalia is a lightweight Wayland shell and bar built directly on
Wayland and OpenGL ES, with no Qt or GTK dependency.")
    (license license:expat)))
