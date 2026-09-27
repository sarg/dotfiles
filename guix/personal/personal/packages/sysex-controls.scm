(define-module (personal packages sysex-controls)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public sysex-controls
  (package
    (name "sysex-controls")
    (version "0.2.28")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/soyersoyer/sysex-controls")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09px56nqm78fmh3ysc292dlz2q21ikpf1mcd2a9ddvq02p9mmx8d"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config gettext-minimal
           desktop-file-utils             ; for update-desktop-database
           `(,gtk+ "bin")                 ; For gtk-update-icon-cache
           `(,glib "bin"))) ; for glib-compile-resources
    (inputs
     (list gtk libadwaita alsa-lib))
    (synopsis "Midi controller configuration tool")
    (home-page "https://github.com/soyersoyer/sysex-controls")
    (description "Configure a few MIDI controllers")
    (license license:gpl3)))
