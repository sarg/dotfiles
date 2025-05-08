(define-module (personal services screen-locker)
  #:use-module (guix gexp)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (personal services utils)
  #:use-module (personal packages next)

  #:export (video-saver image-saver screen-locker))

(define (video-saver file)
  (chmod-computed-file
   (mixed-text-file "mpv-saver"
                    "#!/bin/sh\n"
                    "exec " mpv "/bin/mpv --terminal=no"
                    " --loop --fullscreen --osc=no --no-stop-screensaver"
                    " --osd-bar=no --no-config --no-input-default-bindings"
                    " --wid=${XSCREENSAVER_WINDOW} " file)
   #o555))

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
                    " exec " xsecurelock-next "/bin/xsecurelock")
   #o555))

(define (screen-locker saver)
  (let* ((xss-lock (file-append xss-lock "/bin/xss-lock"))
         (dimmer (file-append xsecurelock-next "/libexec/xsecurelock/dimmer")))

    (chmod-computed-file
     (mixed-text-file "screen-locker"
                      "#!/bin/sh\n"
                      "xset s 300 5\n"
                      "exec " xss-lock " -n " dimmer " -l " (xsecurelock-wrapper saver))
     #o555)))
