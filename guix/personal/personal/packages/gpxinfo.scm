(define-module (personal packages gpxinfo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix download))

(define-public python-gpxpy
  (package
    (name "python-gpxpy")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gpxpy" version))
       (sha256
        (base32 "1bh1dkrbmcqb46r7j4fazzq7j6zfr2f04frm6h4bhhpcjx5lhb57"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/tkrajina/gpxpy")
    (synopsis "GPX file parser and GPS track manipulation library")
    (description "GPX file parser and GPS track manipulation library")
    (license license:asl2.0)))

(define-public python-gpx-cmd-tools
  (package
    (name "python-gpx-cmd-tools")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gpx-cmd-tools" version))
       (sha256
        (base32 "173cksbjid9bpa95s39p2g6qvm0i8jdyxmdr1757brs1kcy2zp0d"))))
    (build-system pyproject-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs (list python-gpxpy))
    (home-page "https://github.com/tkrajina/gpx-cmd-tools")
    (synopsis "Set of GPX command-line utilities")
    (description "Set of GPX command-line utilities")
    (license license:asl2.0)))

(define-public emacs-gpx
  (let ((commit "b7cfc0f7ec53808f48c070f9c811934a7afcc580")
        (revision "1"))
    (package
      (name "emacs-gpx")
      (version (git-version "0.1.0" revision commit))
      (home-page "https://github.com/mkcms/gpx-mode")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zndib63yj88g9hqnnjya5x9bgfc8qw75phfqbhbpgi99ip25lxx"))))
      (build-system emacs-build-system)
      (arguments '(#:include '("\\.el$" "\\.py$")))
      (propagated-inputs
       (list python-gpx-cmd-tools python-matplotlib python-folium))
      (synopsis "Helm action to switch directory in Emacs REPLs")
      (description "Helm \"Switch-to-REPL\" offers the
@code{helm-switch-to-repl} action, a generalized and extensible version of
@code{helm-ff-switch-to-shell}.  It can be added to @code{helm-find-files} and
other @code{helm-type-file} sources such as @code{helm-locate}.")
      (license license:gpl3+))))

emacs-gpx
