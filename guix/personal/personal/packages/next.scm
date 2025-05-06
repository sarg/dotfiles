(define-module (personal packages next)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages xorg))

;; supports tearfree in modesetting driver
(define-public xorg-server-next
  (package
    (inherit xorg-server-xwayland)
    (name "xorg-server-next")
    (version "22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/xorg/xserver.git")
             (commit "37b7ea8f8aaef3efa9d56fb9bc82adeccba02633")))
       (sha256
        (base32
         "0sfji9lhp55wfibykv2sqckr3ax2kxssd4qd196wm7pqxbrxaf5k"))))
    (inputs
     (append
      (package-inputs xorg-server-xwayland)
      (package-inputs xorg-server)))
    (propagated-inputs
     (package-propagated-inputs xorg-server))))

(define-public xf86-input-libinput-next
  (package
    (inherit xf86-input-libinput)
    (inputs
     (modify-inputs (package-inputs xf86-input-libinput)
       (replace "xorg-server" xorg-server-next)))))

(define* (xorg-start-command-xinit #:optional (config (xorg-configuration)))
  "Return a @code{startx} script in which the modules, fonts, etc. specified
in @var{config}, are available.  The result should be used in place of
@code{startx}.  Compared to the @code{xorg-start-command} it calls xinit,
therefore it works well when executed from tty."
  (define X
    (xorg-wrapper config))

  (define exp
    ;; Small wrapper providing subset of functionality of typical startx
    ;; script from distributions like alpine.
    (with-imported-modules (source-module-closure '((guix build utils)))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 textual-ports))

          (define (capture-stdout . prog+args)
            (let* ((port (apply open-pipe* OPEN_READ prog+args))
                   (data (get-string-all port)))
              (if (zero? (status:exit-val (close-pipe port)))
                  (string-trim-right data #\newline)
                  (error "Command failed: " prog+args))))

          (define (determine-unused-display n)
            (let ((lock-file (format #f "/tmp/.X~a-lock" n))
                  (sock-file (format #f "/tmp/.X11-unix/X~a" n)))
              (if (or (file-exists? lock-file)
                      (false-if-exception
                       (eq? 'socket (stat:type (stat sock-file)))))
                  (determine-unused-display (+ n 1))
                  (format #f ":~a" n))))

          (define (vty-from-fd0)
            (let ((fd0 (readlink "/proc/self/fd/0"))
                  (pref "/dev/tty"))
              (if (string-prefix? pref fd0)
                  (substring fd0 (string-length pref))
                  (error (format #f "Cannot determine VT from: ~a" fd0)))))

          (define (determine-vty)
            (string-append "vt" (or (getenv "XDG_VTNR") (vty-from-fd0))))

          (define (enable-xauth server-auth-file display)
            ;; Configure and enable X authority
            (or (getenv "XAUTHORITY")
                (setenv "XAUTHORITY" (string-append (getenv "HOME") "/.Xauthority")))

            (let* ((bin/xauth #$(file-append xauth "/bin/xauth"))
                   (bin/mcookie #$(file-append util-linux "/bin/mcookie"))
                   (mcookie (capture-stdout bin/mcookie)))
              (invoke bin/xauth "-qf" server-auth-file
                      "add" display "." mcookie)
              (invoke bin/xauth "-q"
                      "add" display "." mcookie)))

          (let* ((xinit #$(file-append xinit "/bin/xinit"))
                 (display (determine-unused-display 0))
                 (vty (determine-vty))
                 (server-auth-port (mkstemp "/tmp/serverauth.XXXXXX"))
                 (server-auth-file (port-filename server-auth-port)))
            (close-port server-auth-port)
            (enable-xauth server-auth-file display)
            (apply execl
                   xinit
                   xinit
                   "--"
                   #$X
                   display
                   vty
                   "-keeptty"
                   "-auth" server-auth-file
                   ;; These are set by xorg-start-command, so do the same to keep
                   ;; it consistent.
                   "-logverbose" "-verbose" "-terminate"
                   #$@(xorg-configuration-server-arguments config)
                   (cdr (command-line)))))))

  (program-file "startx" exp))

(define-public startx
  (xorg-start-command-xinit
   (xorg-configuration
    (server xorg-server-next)
    (modules (list xf86-input-libinput-next)))))

(define-public emacs-calibredb-next
  (let ((commit "40a16be1391e526c0a96e6eda3ba6bd44778e5d8")
        (revision "0"))
    (package
      (inherit emacs-calibredb)
      (name "emacs-calibredb")
      (version (git-version "2.13.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chenyanming/calibredb.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1a0a119fr4xznscjsgbjfmgn4xsp7kj4ylr5xl4s0xlqjcxjk4yr")))))))


(define-public xsecurelock-next
  (package
    (inherit xsecurelock)
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/google/xsecurelock/releases"
             "/download/v" version "/xsecurelock-" version ".tar.gz"))
       (sha256
        (base32 "09c0br8vwx9q728i4iv1pcp4s0sm0cd1c5ligag4k2730kcg93bf"))))))
