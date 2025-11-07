(define-module (personal packages pam)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages linux))

(define-public pam-fprint-grosshack
  (package
    (name "pam-fprint-grosshack")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/mishakmak/pam-fprint-grosshack.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b92300sw8mxpjb51ik72rids837nxyafvx534qpx87znxjk7dx1"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dpam_modules_dir=" %output "/lib/security"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enable-elogind
            (lambda _
              (substitute* "pam/meson.build"
                (("libsystemd") "libelogind"))
              (substitute* "meson.build"
                (("libsystemd") "libelogind")))))))
    (native-inputs (list pkg-config cmake))
    (inputs (list dbus-glib linux-pam pam-wrapper libfprint elogind polkit))
    (home-page "https://gitlab.com/mishakmak/pam-fprint-grosshack")
    (description "This is a fork of the pam module which implements the simultaneous
password and fingerprint authentication.")
    (synopsis "PAM module that allows either a password or fingerprint authentication.")
    (license license:gpl2+)))
