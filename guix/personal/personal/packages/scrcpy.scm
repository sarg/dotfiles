(define-module (personal packages scrcpy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages android)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages))

(define scrcpy-server
  (let* ((version "1.25"))
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/Genymobile/scrcpy"
                         "/releases/download/v" version "/"
                         "scrcpy-server-v" version))
     (sha256
      (base32
       "1nwvxixbdv0k72lf0wfycmd9jx1pwc7frxq6dlpyfsnhpg3hc0yf")))))

(define-public scrcpy
  (package
   (name "scrcpy")
   (version "1.25")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Genymobile/scrcpy")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1qyrwsx53xky2a5nzssz95gpz5mfb7br6vzw99ckichyia3c4kz1"))))
   (build-system meson-build-system)
   (native-inputs (list pkg-config))
   (inputs (list ffmpeg sdl2 libusb scrcpy-server adb))
   (arguments
    `(#:configure-flags (list "-Dcompile_server=false")
      #:phases (modify-phases %standard-phases
                              (add-after 'install 'wrap-executable-with-adb-path
                                         (lambda* (#:key inputs outputs #:allow-other-keys)
                                           (let* ((out (assoc-ref outputs "out"))
                                                  (jar (assoc-ref inputs "_"))
                                                  (adb (assoc-ref inputs "adb")))
                                             (wrap-program (string-append out "/bin/scrcpy")
                                               `("ADB" = (,(string-append adb "/bin/adb")))
                                               `("SCRCPY_SERVER_PATH" = (,jar)))))))))
   (home-page "https://github.com/Genymobile/scrcpy")
   (synopsis "Display and control your Android device")
   (description "Scrcpy (screen copy) provides display and control of Android devices connected on USB (or over TCP/IP). It does not require any root access.")
   (license license:asl2.0)))

scrcpy
