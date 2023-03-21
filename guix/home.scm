(use-modules
 (gnu)
 (guix)
 (guix gexp)
 (guix modules)
 (guix utils)
 (guix channels)
 (gnu home)
 (gnu services)
 (gnu packages)
 (gnu packages xorg)
 (gnu packages xdisorg)
 (gnu home services)
 (gnu home services shepherd)
 (gnu home services desktop)
 (gnu home services shells)
 (gnu home services guix)
 (srfi srfi-1)
 (srfi srfi-11)
 (ice-9 match)
 (ice-9 ftw)
 (guix packages))

(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync" "plocate"
    "atool" "p7zip" "unzip" "jq"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz"
    "bind:utils"                        ; dig
    "lshw" "strace" "nftables" "file"))

(define %pkg-desktop
  '("pavucontrol" "dunst" "flameshot"
    "pulseaudio" "polybar"
    "redshift" "unclutter"))

(define %pkg-fonts
  '("font-fira-code"
    "font-openmoji"
    "font-google-noto-emoji"
    "font-hack"
    "font-terminus"))

(define %pkg-x11
  '("picom" "xhost" "xkbcomp" "xkbset"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput"
    "hicolor-icon-theme" "tango-icon-theme"
    "igt-gpu-tools"))

(define %pkg-games
  '(;; "lierolibre" "chroma" "meandmyshadow" "gcompris-qt"
    ;; "tipp10" "quakespasm" "sgt-puzzles" "xonotic"
    "quake3e"))

(define %pkg-apps
  '(;; apps
    "calibre" "anki" "qutebrowser"
    "openvpn" "openssh"
    "mu" "msmtp" "isync"
    "gnupg" "pass-otp" "password-store" "pinentry-emacs" "pwgen"
    "piper"

    ;; "zeal" "qalculate-gtk" "simplescreenrecorder"
    ;; media
    "libreoffice" "qview" "stapler" "gimp"
    "graphicsmagick" "libwebp" "jpegoptim"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu"
    "yt-dlp" "mpv"))

(define %pkg-dev
  '("python" "emacs-debbugs" "gnu-standards"
    "git" "git:send-email" "git-crypt" "perl" ;for some git commands
    ;; "openjdk:jdk"
    ))

(define (del-prefix p str)
  (if (string-prefix? p str)
      (substring/shared str (string-length p))
      str))

(define* (as-local-files dir #:optional (trim-prefix dir))
  (let ((absolute-dir (string-append (current-source-directory) "/" dir))
        (to-trim (string-append (current-source-directory) "/" trim-prefix "/")))
    (map (lambda (fn)
           (list
            (del-prefix to-trim fn)
            (local-file (canonicalize-path fn) (del-prefix "." (basename fn)) #:recursive? #t)))
         (file-system-fold
          (lambda (path stat result) #t)
          (lambda (path stat result) (cons path result))
          (lambda (name stat result) result)
          (lambda (name stat result) result)
          (lambda (name stat result) result)
          (lambda (name stat errno result) result)
          '() absolute-dir))))

(define symlinks
  '(("Sync")
    ("Resources")
    ("devel")
    ("devel/dotfiles" ".dotfiles")
    ("Sync/pass" ".password-store")
    ("data/telega" ".telega")
    ("data/mail" ".mail")
    ("data/gnupg" ".gnupg")
    ("data/events" ".events")
    ("data/syncthing" ".config/syncthing")
    ("data/qutebrowser" ".local/share/qutebrowser")
    ("devel/dotfiles/emacs/.config/emacs" ".config/emacs")
    ("devel/dotfiles/emacs/.doom.d" ".doom.d")
    ("apps/quake3" ".q3a")))

(define symlinks-activation
  (with-imported-modules (source-module-closure
                          '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))

        (define (no-follow-file-exists? file)
          "Return #t if file exists, even if it's a dangling symlink."
          (->bool (false-if-exception (lstat file))))

        (let ((home (string-append (getenv "HOME") "/"))
              (storage "/storage/"))
          (for-each
           (lambda (s)
             (let ((source-file (string-append storage (first s)))
                   (target-file (string-append home (last s))))

               (unless (no-follow-file-exists? target-file)
                 (symlink source-file target-file))))

           '#$symlinks)))))

(define %emacs-home (load "./emacs-home.scm"))
(home-environment
 (packages
  (append (home-environment-packages %emacs-home)
          (map (compose list specification->package+output)
               (append %pkg-android
                       %pkg-games
                       %pkg-utils
                       %pkg-desktop
                       %pkg-fonts
                       %pkg-x11
                       %pkg-dev
                       %pkg-apps))))

 (services
  (append
   (home-environment-user-services %emacs-home)
   (list (service home-bash-service-type)

         (service home-xorg-server-service-type
                  (xorg-configuration
                   (modules (list xf86-video-intel xf86-input-libinput))
                   (drivers (list "intel"))))

         (simple-service
          'sx-autostart home-bash-service-type
          (home-bash-extension
           (bash-profile
            (list (mixed-text-file
                   "sx-autostart"
                   "[[ ! $DISPLAY && $(tty) == /dev/tty1 ]] && exec " sx "/bin/sx")))))

         (service home-dbus-service-type)
         (simple-service 'symlinks
                         home-activation-service-type
                         symlinks-activation)

         (simple-service 'configs
                         home-files-service-type
                         (append
                          (as-local-files "../backup")
                          (as-local-files "../android")
                          (as-local-files "../email")
                          (as-local-files "../xsession")
                          (as-local-files "../git")
                          (as-local-files "../qutebrowser")
                          (as-local-files "../desktop")
                          `((".gtkrc-2.0.mine"
                             ,(plain-file "gtk.conf"
                                          "gtk-icon-theme-name=\"Tango\"")))
                          `((".gnupg/gpg-agent.conf"
                             ,(mixed-text-file "gpg-agent.conf"
                                               "enable-ssh-support\n"
                                               "allow-emacs-pinentry\n"
                                               "pinentry-program "
                                               (specification->package "pinentry-emacs")
                                               "/bin/pinentry-emacs\n"
                                               "default-cache-ttl 86400\n"
                                               "max-cache-ttl 86400\n"))
                            (".gnupg/gpg.conf"
                             ,(mixed-text-file "gpg.conf"
                                               "keyid-format 0xlong\n")))))

         (simple-service 'extra-channels
                         home-channels-service-type
                         (list
                          (channel
                           (name 'nonguix)
                           (url "https://gitlab.com/nonguix/nonguix")
                           ;; Enable signature verification:
                           (introduction
                            (make-channel-introduction
                             "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                             (openpgp-fingerprint
                              "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

                          (channel
                           (name 'personal)
                           (url (string-append "file://" (dirname (current-filename)) "/personal")))))

         ;; (service home-shepherd-service-type
         ;;          (home-shepherd-configuration
         ;;           (services
         ;;            (list minidlna-service))))

         (simple-service 'additional-env-vars-service
                         home-environment-variables-service-type
                         `(("PATH" . "$HOME/.local/bin:$HOME/.config/emacs/bin:$PATH")
                           ("SSH_AUTH_SOCK" . "$(gpgconf --list-dir agent-ssh-socket)")
                           ("VISUAL" . "emacsclient")
                           ("EDITOR" . "emacsclient")))))))
