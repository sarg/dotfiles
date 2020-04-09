(define-module (personal packages emacs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs))

(define-public emacs-next-snapshot
  (let ((commit "1aeb1819353418ebed635f18a009048700ba1ad0")
        (revision "0")
        (emacs-version "28.0.50"))

    (package
      (inherit emacs-next)
      (name "emacs-next-snapshot")
      (version (git-version emacs-version revision commit))
      (source
       (origin
         (inherit (package-source emacs-next))

         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacs.git")
               (commit commit)))

         (file-name (git-file-name name version))
         (sha256
          (base32 "1ij6x5dlrqjpm3ml0a0lbb41wsq2rpbv29nlspz452i8ncjfiaym"))))

      (native-search-paths
       (list (search-path-specification
              (variable "EMACSLOADPATH")
              ;; The versioned entry is for the Emacs' builtin libraries.
              (files
               (list "share/emacs/site-lisp"
                     (string-append "share/emacs/" emacs-version "/lisp"))))


             (search-path-specification
              (variable "INFOPATH")
              (files '("share/info"))))))))
