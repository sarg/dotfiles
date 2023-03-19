(define-module (personal packages xdisorg)
  #:use-module (gnu)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xorg))

(define-public physlock
  (package
    (name "physlock")
    (version "13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xyb3rt/physlock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mz4xxjip5ldiw9jgfq9zvqb6w10bcjfx6939w1appqg8f521a7s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)

         (add-after 'unpack 'fix-makefile
           (lambda _
             (substitute* "Makefile" (("-m 4755 -o root -g root") "")))))

       #:make-flags
       (list "HAVE_SYSTEMD=0" "HAVE_ELOGIND=1"
             (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))))
    (native-inputs (list linux-pam elogind))
    (synopsis
     "Control physical access to a linux computer by locking all of its
virtual terminals.")
    (description
     "physlock only allows the user of the active session (the user logged in
on the foreground virtual terminal) and the root user to unlock the computer
and uses PAM for authentication.

physlock uses 3 mechanisms to detect the user of the active session:
- Querying elogind(8)
- Searching the utmp file for an entry whose ut_line field is the base name of
the active tty device file
- Using the owner of the active tty device file typically set by login(1)")
    (home-page "https://github.com/xyb3rt/physlock")
    (license license:gpl2)))

physlock
