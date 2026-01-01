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
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (guix download))

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
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-gpxpy))
    (home-page "https://github.com/tkrajina/gpx-cmd-tools")
    (synopsis "Set of GPX command-line utilities")
    (description "Set of GPX command-line utilities")
    (license license:asl2.0)))

(define-public emacs-gpx
  (package
    (name "emacs-gpx")
    (version "0.2.0")
    (home-page "https://github.com/mkcms/gpx-mode")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w0651cln7pnvb3fj8x98djnmg14b2lh17qn7lpxqr1s41d53i4c"))))
    (build-system emacs-build-system)
    (arguments
     (list #:include #~(list "\\.el$" "\\.py$")
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'configure-path
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (emacs-substitute-variables "gpx.el"
                       ("gpx-gpxinfo-executable"
                        (search-input-file inputs "/bin/gpxinfo"))))))
               (add-after 'install 'wrap-scripts
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (for-each
                    (lambda (f)
                      (substitute* f
                        (("import sys\n" all)
                         (apply string-append
                                all
                                (map (lambda (path)
                                       (string-append "sys.path.append('" path "')\n"))
                                     (string-split (getenv "GUIX_PYTHONPATH") #\:))))))
                    (find-files (elpa-directory #$output) "\\.py")))))))
    (inputs (list python
                  python-gpx-cmd-tools
                  python-matplotlib
                  python-folium))
    (synopsis "Helm action to switch directory in Emacs REPLs")
    (description "Helm \"Switch-to-REPL\" offers the
@code{helm-switch-to-repl} action, a generalized and extensible version of
@code{helm-ff-switch-to-shell}.  It can be added to @code{helm-find-files} and
other @code{helm-type-file} sources such as @code{helm-locate}.")
    (license license:gpl3+)))

(define-public python-gpxcsv
  (package
    (name "python-gpxcsv")
    (home-page "https://github.com/astrowonk/gpxcsv")
    (version "0.2.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gfgdz1j1iqfg3lqk7dildirpwn4b90n99khrv3krmypahygnsdv"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-lxml))
    (synopsis "GPX file parser and GPS track manipulation library")
    (description "GPX file parser and GPS track manipulation library")
    (license license:asl2.0)))
