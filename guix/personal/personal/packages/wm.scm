;; see noctalia.scm at https://github.com/noctalia-dev/noctalia-shell
(define-module (personal packages wm)

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
  #:use-module (gnu packages cpp)
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
    (version "5.0.0-beta2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noctalia-dev/noctalia-shell")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lmshnybaiwb4s3lmp5jmbq589xlf9sy1bf9nirkk5s258ihgafa"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-for-build
                 (lambda _
                   (substitute* '("meson.build"
                                  "src/render/core/thumbnail_service.cpp"
                                  "src/render/core/image_encoder.cpp"
                                  "src/render/core/stb_image_resize_impl.cpp"
                                  "src/render/core/image_file_loader.cpp"
                                  "src/capture/screenshot_service.cpp")
                     (("stb/stb") "stb"))
                   ;; For reproducibility.
                   (substitute* "meson.build"
                     (("'-march=native', '-mtune=native',") ""))
                   ;; /bin/sh doesn't exist in the build environment.
                   (substitute* "tests/process_test.cpp"
                     (("/bin/(sh)" _ cmd)
                      (which cmd))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           curl
           fontconfig
           freetype
           glib
           gmp
           mpfr
           harfbuzz
           jemalloc
           (librsvg-for-system)
           libqalculate
           libwebp
           libxkbcommon
           libxml2
           linux-pam
           mesa
           md4c
           nlohmann-json
           tomlplusplus
           pango
           pipewire
           polkit
           sdbus-c++
           stb-image-write
           stb-image-resize2
           wayland
           wayland-protocols-1.48
           wireplumber))
    (home-page "https://noctalia.dev/")
    (synopsis "Wayland shell and bar")
    (description
     "Noctalia is a lightweight Wayland shell and bar built directly on
Wayland and OpenGL ES, with no Qt or GTK dependency.")
    (license license:expat)))
