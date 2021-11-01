(use-modules
  (guix transformations)
  (guix gexp)
  (guix store)
  (guix utils)
  (gnu home)
  (gnu home services)
  (gnu home services shepherd)
  (guix build-system copy)
  (gnu services)
  (gnu packages xorg)
  (gnu services xorg)
  (gnu packages)
  (srfi srfi-11)
  (ice-9 match)
  (ice-9 ftw)
  (ice-9 pretty-print)
  (guix packages)
  (gnu packages xdisorg)
  (gnu home services shells))

(define %user "sarg")

(define %pkg-android
  '("adb" "fastboot" "fdroidcl" "socat"
    ;; "scrcpy" pkill9
    ))

(define %pkg-utils
  '("aria2" "curl" "rsync"
    "atool" "p7zip" "unzip" "jq"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz"
    "git" "git:send-email" "git-crypt"
    "bind:utils"                        ; dig

    "lshw" "strace" "nftables" "file"))

(define %pkg-desktop
  '("brightnessctl" "pavucontrol"
    "physlock" "slock" "dunst" "flameshot"
    "pulseaudio" "dbus" "polybar" "redshift" "st"
    "awesome" "wpa-supplicant-gui" "udiskie"

    ;; counsel-linux-app uses gtk-launch
    "gtk+:bin"                 ; for gtk-launch
    "glib:bin"                 ; for gio-launch-desktop which is used by gtk-launch
    ))

(define %pkg-fonts
  '("font-fira-code"
    "font-google-noto"
    "font-hack"
    "font-terminus"))

(define %emacs-next
  ((options->transformation
    '((with-commit . "emacs-next=90bd80d47b7601d8a9a3936a02d085731500c4c9")))
   (specification->package "emacs-next")))

(define %pkg-emacs
  '("avfs"
    "emacs-emacsql"
    "emacs-guix"
    ;; "emacs-next"
    "emacs-pdf-tools"
    "emacs-telega"
    "emacs-telega-contrib"
    "emacs-vterm"))

(define %pkg-x11
  '("picom" "xhost" "xkbcomp" "xkbset"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput"
    "igt-gpu-tools"                     ; intel graphics tool
    ))

(define %pkg-games
  '("lierolibre" "chroma" "meandmyshadow" "gcompris-qt"
    "tipp10" "xonotic" "quake3e" "quakespasm"))

(define %pkg-apps
  '("glibc-utf8-locales"

    ;; apps
    "calibre" ;; "goldendict"
    "anki" "qutebrowser"
    "syncthing" "openvpn" "openssh"
    "mu" "msmtp" "isync"
    "gnupg" "pass-otp" "password-store" "pinentry-tty" "pwgen"
    "beancount" "piper"

    ;; media
    "libreoffice" "twinkle" "qview" "stapler" "gimp" "imagemagick"
    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu"
    "youtube-dl" "mpv" "readymedia"
    "jpegoptim"

    ;; dev
    "openjdk:jdk" "python"))

(define (del-prefix p str)
  (if (string-prefix? p str)
      (substring/shared str (string-length p))
      str))

(define (as-local-files dir)
  (let ((absolute-dir (string-append (current-source-directory) "/" dir)))
    (map (lambda (fn)
           (list
            (del-prefix "." (del-prefix (string-append absolute-dir "/") fn))
            (local-file (canonicalize-path fn) (del-prefix "." (basename fn)))))
         (file-system-fold
          (lambda (path stat result) #t)
          (lambda (path stat result) (cons path result))
          (lambda (name stat result) result)
          (lambda (name stat result) result)
          (lambda (name stat result) result)
          (lambda (name stat errno result) result)
          '() absolute-dir))))

(define my-sx
  (package/inherit sx
    (name "my-sx")
    (inputs
     `(("xwrapper" ,(xorg-start-command
                    (xorg-configuration
                     (modules (list xf86-video-intel
                                    xf86-input-libinput
                                    xf86-input-evdev
                                    xf86-input-keyboard
                                    xf86-input-mouse
                                    xf86-input-synaptics))
                     (drivers (list "intel")))))
       ,@(package-inputs sx)))
    (arguments
     (substitute-keyword-arguments (package-arguments sx)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'refer-to-xorg
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "sx"
                 (("\\bexec Xorg\\b")
                  (string-append "exec " (assoc-ref inputs "xwrapper"))))))))))))

(home-environment
 (packages
  (append (list %emacs-next my-sx)
          (map (compose list specification->package+output)
               (append %pkg-android
                       %pkg-utils
                       %pkg-desktop
                       %pkg-emacs
                       %pkg-fonts
                       %pkg-x11
                       %pkg-apps))))

 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (guix-defaults? #t)
          (bash-profile `(,(mixed-text-file
                            "bash_profile"
                            "[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && "
                            "exec sx " (local-file "../xsession/.xsession" "sxrc") "\n"
                            "export GPG_TTY=$(tty)\n")))))

        (simple-service 'configs
                        home-files-service-type
                        (append
                         (as-local-files "../android")
                         (as-local-files "../email")
                         (as-local-files "../ssh")
                         (as-local-files "../xsession")
                         (as-local-files "../git")
                         (as-local-files "../qutebrowser")
                         (as-local-files "../desktop")
                         `(("gnupg/gpg-agent.conf"
                            ,(mixed-text-file "gpg-agent.conf"
                                              "enable-ssh-support\n"
                                              "allow-emacs-pinentry\n"
                                              "pinentry-program "
                                              (specification->package "pinentry-tty")
                                              "/bin/pinentry-tty\n"
                                              "default-cache-ttl 34560000\n"
                                              "max-cache-ttl 34560000\n")))))

        (service home-shepherd-service-type
                 (home-shepherd-configuration
                  (services
                   (list
                    (shepherd-service
                     (documentation "Run minidlnad")
                     (provision '(minidlnad))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append (specification->package "readymedia") "/sbin/minidlnad")
                                     "-d" "-P" "/tmp/minidlna.pid" "-f"
                                     #$(mixed-text-file "minidlna.conf"
                                                        "media_dir=/home/" %user "/Movies/\n"
                                                        "db_dir=/home/" %user "/.cache/minidlna/\n"
                                                        "log_dir=/home/" %user "/.cache/minidlna/\n"
                                                        "wide_links=yes"))))
                     (stop #~(make-kill-destructor)))))))

        (simple-service 'additional-env-vars-service
                        home-environment-variables-service-type
                        `(("PATH" . "$HOME/.local/bin:$PATH")
                          ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh")
                          ("VISUAL" . "emacsclient")
                          ("EDITOR" . "emacsclient"))))))
