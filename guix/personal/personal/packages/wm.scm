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
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
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
    (properties '((commit . "87db64f1ad856ded71a09bfae971025640a6034d")))
    (version (git-version "5.0.0-beta1" "2" (assoc-ref properties 'commit)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/noctalia-dev/noctalia-shell")
                     (commit (assoc-ref properties 'commit))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15gc5r1h97a9icxwg4xcf8dxwjv7ikcrj0jh06rvvn3q5hwzirjx"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           ;; FIXME: process_test fails with:
           ;; --8<---------------cut here---------------start------------->8---
           ;; stderr:
           ;; process_test: completion-only async command exit code was wrong
           ;; process_test: completion-only async command stdout was wrong
           ;; --8<---------------cut here---------------end--------------->8---
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'prepare-for-build
                 (lambda _
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
           pango
           pipewire
           polkit
           sdbus-c++
           wayland
           wayland-protocols-1.48
           wireplumber))
    (home-page "https://noctalia.dev/")
    (synopsis "Wayland shell and bar")
    (description
     "Noctalia is a lightweight Wayland shell and bar built directly on
Wayland and OpenGL ES, with no Qt or GTK dependency.")
    (license license:expat)))
