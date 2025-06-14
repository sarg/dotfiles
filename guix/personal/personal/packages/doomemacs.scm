(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public doomemacs
  (let ((commit "e6c755305358412a71a990fc2cf592c629edde1e")
        (revision "20250614"))
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
          (base32 "1f5k6mhrdcfsq1gqc0m36f1zvsi9fwi4cmrvf6jfaf3q9bpyalbg"))))

      (build-system copy-build-system)
      (home-page "https://github.com/doomemacs/doomemacs")
      (description "Doom emacs sources")
      (synopsis "Doom emacs")
      (license license:expat))))
