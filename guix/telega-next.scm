(use-modules
 (guix transformations)
 (guix packages)
 (guix git-download)
 (gnu packages messaging)
 (gnu packages emacs-xyz))


(define %tdlib-transformation
  (options->transformation
   '((with-commit . "tdlib=4ed0b23c9c99868ab4d2d28e8ff244687f7b3144"))))

(define emacs-telega-server-next
  (let ((commit "4f08c835c08e762137ca04e12055cf9dc0b2b8cf")
        (revision "1"))
    (package
      (inherit emacs-telega-server)
      (version (git-version "0.8.203" revision commit))
      (source
       (origin
         (inherit (package-source emacs-telega-server))
         (uri (git-reference
               (url "https://github.com/zevlg/telega.el")
               (commit commit)))
         (sha256
          (base32 "02iv2cxwsmfpx2b6wvp7l22izvqw21f1b98jm0yihmfh39idxwn8")))))))

(define emacs-telega-next
  (package
    (inherit emacs-telega)
    (source (package-source emacs-telega-server-next))
    (version (package-version emacs-telega-server-next))))

(define emacs-telega-contrib-next
  (package
    (inherit emacs-telega-contrib)
    (source (package-source emacs-telega-server-next))
    (version (package-version emacs-telega-server-next))))

(define %telega-server-next
  ((package-input-rewriting
    `((,tdlib . ,(%tdlib-transformation tdlib))))
   emacs-telega-server-next))

(define %telega-next
  ((package-input-rewriting
    `((,emacs-telega-server . ,%telega-server-next)))
   emacs-telega-next))

(define %telega-contrib-next
  ((package-input-rewriting
    `((,emacs-telega . ,%telega-next)))
   emacs-telega-contrib-next))

%telega-next
