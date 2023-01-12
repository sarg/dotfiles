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
    (version "2022-12-31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ec-/Quake3e")
             (commit "f9f923672940f35cd9afc4c0a5f0aa6ddacb3fa6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p2yr67bav0imwfpmqn8d5jwlgz5ycsn6d1ipx9ssld84zhdvpvh"))))
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
