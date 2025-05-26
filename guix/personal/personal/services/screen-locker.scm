(define-module (personal services screen-locker)
  #:use-module (guix gexp)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (personal services utils)

  #:export (video-saver image-saver screen-locker))

(define (video-saver file)
  (string-append "saver_mpv"
   " XSECURELOCK_LIST_VIDEOS_COMMAND='echo " file "'"
   " XSECURELOCK_VIDEOS_FLAGS='--no-config'"))

(define (image-saver file)
  (chmod-computed-file
   (mixed-text-file "feh-saver"
                    "#!/bin/sh\n"
                    "exec " feh "/bin/feh --window-id=${XSCREENSAVER_WINDOW} " file)
   #o555))

(define (xsecurelock-wrapper saver)
  (chmod-computed-file
   (mixed-text-file "xsecurelock"
                    "#!/bin/sh\n"
                    " XSECURELOCK_WANT_FIRST_KEYPRESS=1"
                    " XSECURELOCK_SHOW_KEYBOARD_LAYOUT=0"
                    " XSECURELOCK_PASSWORD_PROMPT=disco"
                    " XSECURELOCK_SHOW_HOSTNAME=0"
                    " XSECURELOCK_SAVER=" saver
                    " exec " xsecurelock "/bin/xsecurelock")
   #o555))

(define (screen-locker saver)
  (let* ((xss-lock (file-append xss-lock "/bin/xss-lock"))
         (dimmer (file-append xsecurelock "/libexec/xsecurelock/dimmer")))

    (chmod-computed-file
     (mixed-text-file "screen-locker"
                      "#!/bin/sh\n"
                      "xset s 300 5\n"
                      "exec " xss-lock " -n " dimmer " -l " (xsecurelock-wrapper saver))
     #o555)))
