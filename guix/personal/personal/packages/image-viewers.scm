(define-module (personal packages image-viewers)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages photo)

  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public nomacs
  (package
    (name "nomacs")
    (version "3.16.224")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nomacs/nomacs.git")
             (commit version)))

       (sha256
        (base32 "05d4hqg0gl3g9s2xf1hr7mc7g4cqarcap4nzxxa51fsphw2b8x16"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DUSE_SYSTEM_QUAZIP=ON")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _ (chdir "ImageLounge") #t)))))

    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("quazip" ,quazip)
       ("opencv" ,opencv)
       ("libraw" ,libraw )
       ("libwebp" ,libwebp)
       ("libtiff" ,libtiff)
       ("exiv2" ,exiv2)
       ("qtbase" ,qtbase)
       ("qtimageformats" ,qtimageformats)
       ("qtsvg" ,qtsvg)
       ("libzip" ,libzip)))
    (synopsis "Qt-based image viewer")
    (description "nomacs is a free, open source image viewer, which supports multiple platforms.
You can use it for viewing all common image formats including RAW and psd images.
It is able to browse images in zip or MS Office files which can be extracted to a directory.
Metadata stored with the image can be displayed and you can add notes to images.
A thumbnail preview of the current folder is included as well as a file explorer panel which allows switching between folders.
nomacs includes image manipulation methods for adjusting brightness, contrast, saturation, hue, gamma, exposure.")
    (home-page "https://nomacs.org/")
    (license license:gpl3+)))

nomacs
