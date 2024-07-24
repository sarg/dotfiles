(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public doomemacs
  (let ((commit "f5b3958331cebf66383bf22bdc8b61cd44eca645")
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
           "19z645fi91r7c3ylygnwsqj0c6jpglg6bx7rqzwwmd5gdzp7aa4w"))))

      (build-system copy-build-system)
      (home-page "https://github.com/doomemacs/doomemacs")
      (description "Doom emacs sources")
      (synopsis "Doom emacs")
      (license license:expat))))

doomemacs
