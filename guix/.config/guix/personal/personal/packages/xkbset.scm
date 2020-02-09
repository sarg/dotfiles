(define-module (personal packages xkbset)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg))

(define-public xkbset
  (package
    (name "xkbset")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://faculty.missouri.edu/~stephen/software/xkbset/xkbset-"
                    version ".tar.gz"))
              (sha256
               (base32 "0wniahjxfbc479lkpvp9460ci3c88722a0pmf3c017mkjna5ghh1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (delete 'configure)
         (add-before 'install 'mkdirs
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             (mkdir-p (string-append (assoc-ref outputs "out") "/usr/share/man/man1"))
             #t)))
       #:make-flags (list (string-append "INSTALL_BIN=" (assoc-ref %outputs "out") "/bin")
                          (string-append "INSTALL_MAN1=" (assoc-ref %outputs "out") "/usr/share/man/man1")
                          "CC=gcc")))
    (native-inputs
     `(("perl" ,perl)
       ("libx11" ,libx11)))
    (synopsis "Small utility to change the AccessX settings of XKEYBOARD")
    (description "xkbset")
    (home-page "https://faculty.missouri.edu/~stephen/software/#xkbset")
    (license gpl3+)))
