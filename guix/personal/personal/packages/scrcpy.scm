(define-module (personal packages scrcpy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages android)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix build-system copy))

(define-public scrcpy-server
  (package
   (name "scrcpy-server")
   (version "3.3.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/Genymobile/scrcpy"
                                "/releases/download/v" version "/" name
                                "-v" version))
            (file-name "scrcpy-server.jar")
            (sha256
             (base32
              "17h70cimwdhfpkhzdl5mp7zxjhamdmzfrdh655aal02ska627245"))))
   (build-system copy-build-system)
   (home-page "https://github.com/Genymobile/scrcpy")
   (synopsis "Server component for scrcpy")
   (description "This package provides the scrcpy-server.jar file needed by scrcpy
to communicate with Android devices.")
   (license license:asl2.0)))

(define-public scrcpy
  (package
   (name "scrcpy")
   (version "3.3.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Genymobile/scrcpy")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32 "1gn75sqqv0r0zh67rqfqmg5il7ac3lkd2wyy0ji9y7s9fmh7h9z7"))))
   (build-system meson-build-system)
   (native-inputs (list pkg-config scrcpy-server))
   (inputs (list ffmpeg sdl2 libusb adb))
   (arguments
    (list
     #:configure-flags #~(list "-Dcompile_server=false")
     #:phases #~(modify-phases %standard-phases
                  (add-after 'install 'wrap-executable-with-adb-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((jar (search-input-file inputs "/scrcpy-server.jar"))
                             (adb (search-input-file inputs "/bin/adb")))
                        (wrap-program (string-append #$output "/bin/scrcpy")
                          `("ADB" = (,adb))
                          `("SCRCPY_SERVER_PATH" = (,jar)))))))))
   (home-page "https://github.com/Genymobile/scrcpy")
   (synopsis "Display and control your Android device")
   (description "Scrcpy (screen copy) provides display and control of Android devices connected
on USB (or over TCP/IP). It does not require any root access.")
   (license license:asl2.0)))
