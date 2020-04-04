(define-module (personal packages xdisorg)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
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
       #:make-flags (list (string-append "INSTALL_BIN=" %output "/bin")
                          (string-append "INSTALL_MAN1=" %output "/usr/share/man/man1")
                          "CC=gcc")))
    (native-inputs
     `(("perl" ,perl)
       ("libx11" ,libx11)))
    (synopsis "Small utility to change the AccessX settings of XKEYBOARD")
    (description "xkbset")
    (home-page "https://faculty.missouri.edu/~stephen/software/#xkbset")
    (license gpl3+)))

(define-public physlock
  (package
    (name "physlock")
    (version "13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/muennich/physlock/archive/v" version ".tar.gz"))
              (sha256
               (base32 "1mjbfxqngnl57zjw3gnwxf0j4f6a6f8l66x08wg18vwi3rm73r4s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
                      (delete 'configure)

                      (add-after 'unpack 'fix-makefile
                                 (lambda _
                                   (substitute* "Makefile"
                                                (("-m 4755 -o root -g root") ""))
                                   #t)))
       #:make-flags (list "HAVE_SYSTEMD=0" "HAVE_ELOGIND=1" "CC=gcc"
                          (string-append "PREFIX=" %output))))
    (native-inputs
     `(("linux-pam" ,linux-pam)
       ("elogind" ,elogind)))
    (synopsis "Control physical access to a linux computer by locking all of its virtual terminals.")
    (description "physlock only allows the user of the active session (the user logged in on the foreground virtual terminal)\
and the root user to unlock the computer and uses PAM for authentication.

physlock uses 3 mechanisms to detect the user of the active session:
- Querying elogind(8)
- Searching the utmp file for an entry whose ut_line field is the base name of the active tty device file
- Using the owner of the active tty device file typically set by login(1)")
    (home-page "https://github.com/muennich/physlock")
    (license gpl2)))
