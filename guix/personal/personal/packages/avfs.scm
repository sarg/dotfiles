(define-module (personal packages avfs)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression))

(define-public avfs
  (package
    (name "avfs")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/avf/avfs/"
                       version "/avfs-" version ".tar.bz2"))
       (sha256
        (base32 "0ax1zbw4pmggx1b784bfabdqyn39k7109cnl22p69y2phnpq2y9s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--enable-library"
         "--enable-fuse")))
    (native-inputs
     `(("xz" ,xz)
       ("pkg-config" ,pkg-config)
       ("fuse" ,fuse)))
    (synopsis "Virtual filesystem that allows browsing of compressed files")
    (description "AVFS is a system, which enables all programs to look inside gzip, tar, zip, etc. files or view remote (ftp, http, dav, etc.)
files, without recompiling the programs.")
    (home-page "http://avf.sourceforge.net/")
    (license license:gpl2)))

avfs
