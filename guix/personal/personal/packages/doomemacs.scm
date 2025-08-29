(define-module (personal packages doomemacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix packages))

(define-public emacs-ob-cfg
  (package
    (name "emacs-ob-cfg")
    (version "0.1")
    (source (local-file
             (string-append (current-source-directory) "/ob-cfg.el")))
    (build-system emacs-build-system)
    (home-page #f)
    (description "helper for guix literate config")
    (synopsis description)
    (license #f)))

(define-public doomemacs
  (let ((commit "e7a69dcdacf9332698714bf3b5c497e5f0653730")
        (revision "20250829"))
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
          (base32 "0hzmi5ajghl7hhly2mmz9b3a10bvfbgfz62jv8f26fksqbmyixig"))))
      (build-system copy-build-system)
      (home-page "https://github.com/doomemacs/doomemacs")
      (description "Doom emacs sources")
      (synopsis "Doom emacs")
      (license license:expat))))
