(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public doomemacs
  (let ((commit "7bc39f2c1402794e76ea10b781dfe586fed7253b") ; <2024-11-14 Thu>
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
           "1hb8rhvrsiiyxwap40swgxqb4zg2zgr2l8jdzxidcjcw4d99yl0p"))))

      (build-system copy-build-system)
      (home-page "https://github.com/doomemacs/doomemacs")
      (description "Doom emacs sources")
      (synopsis "Doom emacs")
      (license license:expat))))
