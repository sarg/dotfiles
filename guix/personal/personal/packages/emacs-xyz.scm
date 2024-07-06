(define-module (personal packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public emacs-doom-themes-latest
  (let ((commit "188ab05eefe2bdc46b4464aadb4a52ff9cb42f7f")
        (revision "0"))
    (package
      (inherit emacs-doom-themes)
      (version (git-version "2.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/doomemacs/themes")
                      (commit commit)))
                (file-name (git-file-name "emacs-doom-themes" version))
                (sha256
                 (base32 "08avm5jfp887r2l77s6i0zn72wlv4kp47b2vzv3kywf3d0gkpgp8")))))))

emacs-doom-themes-latest
