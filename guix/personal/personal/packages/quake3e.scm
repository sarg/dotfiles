(define-module (personal packages quake3e)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages image)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (guix download))

(define-public quake3e
  ;; We follow master since it seems that there won't be releases after 1.3.6.
  (package
    (name "quake3e")
    (version "2023-02-12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ec-/Quake3e")
             (commit "47f3b98fd29e1de2c1d62f0121f8258a2761be7c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0psf4ff1d3xrihaah0qrd83sp85v93ghcv15hr95sk53p0h32bv8"))))
    (build-system gnu-build-system)
    (inputs (list sdl2 libjpeg-turbo openal curl opusfile opus libvorbis freetype libogg))
    (native-inputs (list which pkg-config)) ; which used to find SDL_version.h
    (arguments
     `(#:tests? #f                     ; No tests.
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "DESTDIR=" (assoc-ref %outputs "out") "/bin")
                          "BUILD_SERVER=0"
                          "USE_INTERNAL_LIBS=0"
                          "USE_RENDERER_DLOPEN=0"
                          "USE_SYSTEM_JPEG=1"
                          "USE_VULKAN=0"
                          "USE_VULKAN_API=0"
                          "USE_CURL_DLOPEN=0")
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://ioquake3.org/")
    (synopsis "FPS game engine based on Quake 3")
    (description "ioquake3 is a free software first person shooter engine
based on the Quake 3: Arena and Quake 3: Team Arena source code.  Compared to
the original, ioquake3 has been cleaned up, bugs have been fixed and features
added.  The permanent goal is to create the open source Quake 3 distribution
upon which people base their games, ports to new platforms, and other
projects.")
    (license license:gpl2)))

quake3e
