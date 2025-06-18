(define-module (personal packages owntracks)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages curl)
  #:use-module (guix download))

(define-public owntracks-recorder
  (package
    (name "owntracks-recorder")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/owntracks/recorder")
             (commit "fb3427102501cd509a8f79ef9fb9c38d343645c5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "096f1cm8jk71yzl3zxnsxm60jqc72ssnpxzjyd25px5045q3ch25"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "DOCROOT=" #$output "/share/owntracks/recorder/htdocs")
              (string-append "TZDATADB=" #$output "/share/owntracks/recorder/timezone16.bin"))
      #:phases
      #~(modify-phases %standard-phases
                       (replace 'configure
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (copy-file "config.mk.in" "config.mk")))
                       (replace 'install
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let ((bin (string-append #$output "/bin"))
                                        (share (string-append #$output "/share/owntracks/recorder")))
                                    (mkdir-p bin)
                                    (chmod "ot-recorder" #o755)
                                    (install-file "ot-recorder" bin)
                                    (chmod "ocat" #o755)
                                    (install-file "ocat" bin)

                                    (mkdir-p share)
                                    (install-file "contrib/tzdatadb/timezone16.bin" share)
                                    (copy-recursively "docroot" (string-append share "/htdocs"))))))))

    (inputs (list mosquitto curl lmdb libconfig `(,util-linux "lib")))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/owntracks")
    (synopsis "OwnTracks recorder")
    (description "The OwnTracks Recorder is a lightweight program for storing and accessing
location data published via MQTT (or HTTP) by the OwnTracks apps.")
    (license license:gpl2)))
