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
  #:use-module (guix packages))

(define-public scrcpy
  (package
   (name "scrcpy")
   (version "3.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Genymobile/scrcpy")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32 "0janx96lv15673yfn3zys1x5i70463icmxg3qcgdkapgbp0fa6sz"))))
   (build-system meson-build-system)
   (native-inputs (list pkg-config
                        (origin
                          (method url-fetch)
                          (uri (string-append "https://github.com/Genymobile/scrcpy"
                                              "/releases/download/v" version "/"
                                              "scrcpy-server-v" version))
                          (file-name (string-append "scrcpy-server-" version ".jar"))
                          (sha256
                           (base32
                            "1h465fr3rlhniirml5yahaw086jchhafps8n7brv28rglr20k3wm")))))
   (inputs (list ffmpeg sdl2 libusb adb))
   (arguments
    (list
     #:configure-flags #~(list "-Dcompile_server=false")
     #:phases #~(modify-phases %standard-phases
                  (add-after 'install 'wrap-executable-with-adb-path
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((jar (format #f "scrcpy-sever-~a.jar" #$version))
                             (adb (search-input-file inputs "/bin/adb")))
                        (wrap-program (string-append #$output "/bin/scrcpy")
                          `("ADB" = (,adb))
                          `("SCRCPY_SERVER_PATH" = (,jar)))))))))
   (home-page "https://github.com/Genymobile/scrcpy")
   (synopsis "Display and control your Android device")
   (description "Scrcpy (screen copy) provides display and control of Android devices connected
on USB (or over TCP/IP). It does not require any root access.")
   (license license:asl2.0)))
