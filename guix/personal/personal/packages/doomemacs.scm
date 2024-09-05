(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public doomemacs
  (let ((commit "0c8dff66de394f94f4f1b2225aa9575319db926e")
        (revision "0"))
    (package
      (name "doomemacs")
      (version (git-version "3.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/doomemacs/doomemacs")
               (commit commit)))
         (file-name (git-file-name "doomemacs" version))
         (sha256
          (base32
           "0iq2lsq4rqkyj3rsgf3dxhvgris3wcpd1bvb8iqqqblp2rxjzvwm"))))

      (build-system copy-build-system)
      (home-page "https://github.com/doomemacs/doomemacs")
      (description "Doom emacs sources")
      (synopsis "Doom emacs")
      (license license:expat))))

doomemacs
