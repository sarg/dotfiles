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
  (gnu packages)
  (srfi srfi-11)
  (ice-9 match)
  (ice-9 ftw)
  (ice-9 pretty-print)
  (guix packages)
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
    "git" "git:send-email"
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
  '("xf86-input-libinput" "xf86-video-intel" "picom"
    "xhost" "xkbcomp" "xkbset" "xorg-server"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput" "xauth"
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

(define xorg-conf-intel
  (package
    (name "xorg-conf-intel")
    (version "1")
    (synopsis "xorg conf for intel driver")
    (description "")
    (license "")
    (home-page "")
    (source (mixed-text-file "intel.conf"
                              "Section \"Device\"\n"
                              "  Identifier \"Intel Graphics\"\n"
                              "  Driver \"intel\"\n"
                              "EndSection\n"))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `((,(assoc-ref %build-inputs "source")
          "share/X11/xorg.conf.d/20-intel.conf"))
       #:phases (modify-phases %standard-phases (delete 'unpack))))))

(home-environment
 (packages
  (cons xorg-conf-intel
        (map (compose list specification->package+output)
             (append %pkg-android
                     %pkg-utils
                     %pkg-desktop
                     %pkg-fonts
                     %pkg-emacs
                     %pkg-x11
                     %pkg-apps))))

 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (guix-defaults? #t)
          (bash-profile (list (plain-file "bash_profile"
                                          "[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec $HOME/start.sh")))))

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
                                              "max-cache-ttl 34560000\n"))
                           ("config/minidlna.conf"
                            ,(mixed-text-file "minidlna.conf"
                                              "media_dir=/home/" %user "/Movies/\n"
                                              "db_dir=/home/" %user "/.cache/minidlna/\n"
                                              "log_dir=/home/" %user "/.cache/minidlna/\n"
                                              "wide_links=yes")))))


        (service home-shepherd-service-type
                 (home-shepherd-configuration
                  (services
                   (list
                    (shepherd-service
                     (documentation "Run minidlnad")
                     (provision '(minidlnad))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append (specification->package "readymedia") "/sbin/minidlnad")
                                     "-d" "-P" "/tmp/minidlna.pid" "-f" "/home/sarg/.config/minidlna.conf")))
                     (stop #~(make-kill-destructor)))))))

        (simple-service 'additional-env-vars-service
                        home-environment-variables-service-type
                        `(("PATH" . "$HOME/.local/bin:$PATH")
                          ("VISUAL" . "emacsclient")
                          ("EDITOR" . "emacsclient"))))))
