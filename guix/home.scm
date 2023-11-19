(use-modules
 (gnu)
 (guix)
 (guix gexp)
 (guix build utils)
 (guix utils)
 (guix modules)
 (guix channels)
 (gnu home)
 (gnu services)
 (gnu packages)
 (gnu packages gnupg)
 (gnu home services)
 (gnu home services syncthing)
 (gnu home services shepherd)
 (gnu home services gnupg)
 (gnu home services desktop)
 (gnu home services shells)
 (gnu home services guix)
 (personal services symlinks)
 (personal services sound)
 (personal utils)
 (srfi srfi-1)
 (srfi srfi-11))

(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync" "plocate"
    "atool" "p7zip" "unzip" "jq"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz" "borgmatic" "bind:utils"  ; dig
    "flatpak" "flatpak-xdg-utils" "xdg-desktop-portal"
    "lshw" "strace" "nftables" "file" "lsof"))

(define %pkg-desktop
  '("pavucontrol" "dunst" "flameshot" "wireplumber"
    "alsa-utils" "polybar" "lemonbar-xft" "xss-lock"
    "redshift" "unclutter" "playerctl"))

(define %pkg-fonts
  '("font-fira-code"
    "font-google-noto-emoji" ; 💪
    "font-hack"
    "font-terminus"))

(define %pkg-x11
  '("picom" "xhost" "xkbcomp" "xkbset" "xkb-switch"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput"
    "hicolor-icon-theme" "tango-icon-theme" "adwaita-icon-theme"
    "greybird-gtk-theme"))

(define %pkg-games
  '(;; "lierolibre" "chroma" "meandmyshadow" "gcompris-qt"
    ;; "tipp10" "quakespasm" "sgt-puzzles" "xonotic"
    "quake3e"))

(define %pkg-apps
  '(;; apps
    "calibre" "anki" "qutebrowser"
    "openvpn" "openssh"
    "mu" "msmtp" "isync"
    "gnupg" "pass-otp" "password-store" "pwgen"
    "piper"

    ;; "zeal" "qalculate-gtk" "simplescreenrecorder"
    ;; media
    "libreoffice" "qview" "stapler" "gimp"
    "graphicsmagick" "libwebp" "jpegoptim"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu" "zathura-cb"
    "yt-dlp" "mpv"))

(define %pkg-dev
  '("python" "emacs-debbugs" "gnu-standards"
    "git" "git:send-email" "git-crypt" "perl" ;for some git commands
    "libvirt" "virt-manager"
    ;; "openjdk:jdk"
    ))

(define symlinks
  (resolve-relative-to "/storage/"
   '(("Sync")
     ("Resources")
     ("devel")
     ("devel/dotfiles" . ".dotfiles")
     ("Sync/pass" . ".password-store")
     ("data/telega" . ".telega")
     ("data/mail" . ".mail")
     ("data/gnupg" . ".gnupg")
     ("data/events" . ".events")
     ("data/syncthing" . ".config/syncthing")
     ("data/qutebrowser" . ".local/share/qutebrowser")
     ("devel/dotfiles/emacs/.config/emacs" . ".config/emacs")
     ("devel/dotfiles/emacs/.doom.d" . ".doom.d")
     ("apps/quake3" . ".q3a"))))

(define (sx-autostart-on tty)
  (simple-service
   'sx-autostart home-shell-profile-service-type
   (list (mixed-text-file
          "sx-autostart"
          "[[ ! $DISPLAY && $(tty) == /dev/" tty " ]] && "
          "exec " (@ (gnu packages xdisorg) sx) "/bin/sx"))))

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

         (sx-autostart-on "tty1")

         (service home-pipewire-service-type)
         (service home-symlinks-service-type symlinks)
         (service home-syncthing-service-type)
         (service home-dbus-service-type)

         (service home-gpg-agent-service-type
                  (home-gpg-agent-configuration
                   (pinentry-program
                    (file-append pinentry-emacs "/bin/pinentry-emacs"))
                   (ssh-support? #t)
                   (default-cache-ttl 86400)
                   (max-cache-ttl 86400)))

         (simple-service 'configs
                         home-files-service-type
                         (append!
                          (with-directory-excursion (current-source-directory)
                            `(,@(as-local-files "../backup")
                              ,@(as-local-files "../android")
                              ,@(as-local-files "../email")
                              ,@(as-local-files "../xsession")
                              ,@(as-local-files "../git")
                              ,@(as-local-files "../qutebrowser")
                              ,@(as-local-files "../desktop")))
                          `((".config/gtk-3.0/settings.ini"
                             ,(mixed-text-file "gtk3.conf"
                                               "[Settings]\n"
                                               "gtk-theme-name=Greybird\n"
                                               "gtk-icon-theme-name=\"Adwaita\"\n"
                                               "gtk-font-name=\"Fira Code 12\"\n"
                                               "gtk-cursor-theme-name=\"Adwaita\""))

                            (".config/flameshot/flameshot.ini"
                             ,(mixed-text-file "flameshot.ini"
                                               "[General]\n"
                                               "disabledTrayIcon=true"))

                            (".gnupg/gpg.conf"
                             ,(mixed-text-file "gpg.conf" "keyid-format 0xlong\n")))))

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

         (simple-service 'additional-env-vars-service
                         home-environment-variables-service-type
                         `(("PATH" . "$HOME/.local/bin:$HOME/.config/emacs/bin:$PATH")
                           ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
                           ("DOOMLOCALDIR" . "$HOME/.local/doom/")
                           ("VISUAL" . "emacsclient")
                           ("EDITOR" . "emacsclient")))))))
