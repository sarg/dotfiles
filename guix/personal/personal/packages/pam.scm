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
  #:use-module (gnu packages wm)
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

(define-public swaylock-fprintd
  (let ((commit "536d9dff795eb85720fc942da13e93bebea9f5fa"))
    (package
      (inherit swaylock)
      (name "swaylock-fprintd")
      (version (git-version "0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/SL-RU/swaylock-fprintd")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07hziksn74a61bjv0c7lblvfgad4lc51pgxh7pfw985rbx3m2prk"))))
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'fix-dbus
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "fingerprint/meson.build"
                       (("/usr") (assoc-ref inputs "fprintd"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs swaylock)
         (append `(,glib "bin")))) ; for gdbus-codegen
      (inputs (modify-inputs (package-inputs swaylock)
                (append fprintd dbus))))))
