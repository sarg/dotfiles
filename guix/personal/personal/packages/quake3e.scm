(define-module (personal packages quake3e)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
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
  (package
    (name "quake3e")
    (version "2024-06-21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ec-/Quake3e")
             (commit "ee60b5d736cff2e5e21d347292e9e9f0f640e3b1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sh175f8c26vgdajyczqyh8x2ddl9ff7q9n3hnfcwww3sgmmrdvp"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled copy of these libraries.
           (delete-file-recursively "code/libcurl")
           (delete-file-recursively "code/libogg")
           (delete-file-recursively "code/libsdl")
           (delete-file-recursively "code/libvorbis")
           (delete-file-recursively "code/libjpeg")))))
    (build-system gnu-build-system)
    (inputs (list sdl2 libjpeg-turbo openal curl opusfile opus libvorbis freetype libogg))
    (native-inputs (list which pkg-config)) ; which used to find SDL_version.h
    (arguments
     (list #:tests? #f                      ; No tests.
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "DESTDIR=" #$output "/bin")
                   ;; "BUILD_SERVER=0"
                   "USE_RENDERER_DLOPEN=0"
                   "USE_SYSTEM_JPEG=1"
                   "USE_SYSTEM_OGG=1"
                   "USE_SYSTEM_VORBIS=1"
                   "USE_CURL_DLOPEN=0")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'install 'create-desktop-entry
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((share (string-append #$output "/share")))
                     ;; Create desktop file.
                     (let ((apps (string-append share "/applications"))
                           (icons (string-append share "/icons/hicolor/scalable/apps")))
                       (install-file "code/unix/quake3.svg" icons)
                       (make-desktop-entry-file
                        (string-append apps "/quake3e.desktop")
                        #:name "Quake 3 Arena"
                        #:exec "quake3e.x64"
                        #:icon "quake3"
                        #:keywords '("first person shooter" "fps" "3d" "deathmatch"
                                     "ctf" "capture the flag" "quake iii arena" )
                        #:categories '("Game" "ActionGame")))))))))
    (home-page "https://github.com/ec-/Quake3e")
    (synopsis "FPS game engine based on Quake 3")
    (description "This is a modern Quake III Arena engine aimed to be fast, secure and compatible
with all existing Q3A mods. It is based on last non-SDL source dump of ioquake3
with latest upstream fixes applied.")
    (license license:gpl2)))

quake3e
