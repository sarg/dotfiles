(define-module (personal packages pipemixer)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses))

(define-public pipemixer
  (package
    (name "pipemixer")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/heather7283/pipemixer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10v36j9718qrs033x3sryg5w065wsf6xpl7qbfasixzskisav299"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list pipewire ncurses libinih))
    (home-page "https://github.com/heather7283/pipemixer")
    (synopsis "PipeWire volume mixer with ncurses interface")
    (description "Pipemixer is a volume mixer for PipeWire with an ncurses interface.")
    (license license:gpl3+)))
