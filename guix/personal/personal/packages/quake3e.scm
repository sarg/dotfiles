(define-module (personal packages quake3e)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
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
  (let ((commit "862c454cbccd69ef0ec863008d9e013201bdacd4"))
    (package
      (name "quake3e")
      (version "2022-09-09-1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ec-/Quake3e")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1y4alhpnnxvbi0p2zmiav8hc40v97ygpms0gsxh11qyzi2q7r7gz"))))
      (build-system gnu-build-system)
      (inputs
       `(("sdl2" ,sdl2)
         ("libjpeg" ,libjpeg-turbo)
         ("openal" ,openal)
         ("curl" ,curl)
         ("opusfile" ,opusfile)
         ("opus" ,opus)
         ("libvorbis" ,libvorbis)
         ("freetype" ,freetype)
         ("libogg" ,libogg)))
      (native-inputs
       `(("which" ,which)               ; Else SDL_version.h won't be found.
         ("pkg-config" ,pkg-config)))
      (arguments
       '(#:tests? #f                    ; No tests.
         #:make-flags '("CC=gcc"
                        "BUILD_SERVER=0"
                        "USE_INTERNAL_LIBS=0"
                        "USE_RENDERER_DLOPEN=0"
                        "USE_VULKAN=0"
                        "USE_VULKAN_API=0"
                        "USE_CURL_DLOPEN=0")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "install" "CC=gcc"
                        "USE_INTERNAL_LIBS=0" "USE_RENDERER_DLOPEN=0" "USE_CURL_DLOPEN=0" "BUILD_SERVER=0"
                       (string-append "DESTDIR="
                                      (assoc-ref outputs "out")
                                      "/bin")))))))
      (home-page "https://ioquake3.org/")
      (synopsis "FPS game engine based on Quake 3")
      (description "ioquake3 is a free software first person shooter engine
based on the Quake 3: Arena and Quake 3: Team Arena source code.  Compared to
the original, ioquake3 has been cleaned up, bugs have been fixed and features
added.  The permanent goal is to create the open source Quake 3 distribution
upon which people base their games, ports to new platforms, and other
projects.")
      (license license:gpl2))))

quake3e
