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
 (gnu packages qt)
 (gnu home services)
 (gnu home services syncthing)
 (gnu home services dotfiles)
 (gnu home services shepherd)
 (gnu home services gnupg)
 (gnu home services xdg)
 (gnu home services desktop)
 (gnu home services sound)
 (gnu home services shells)
 (gnu home services guix)
 (personal services symlinks)
 (srfi srfi-1)
 (srfi srfi-11))

(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync" "plocate"
    "atool" "p7zip" "unzip" "jq" "openssh"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz" "borgmatic" "bind:utils"  ; dig
    "graphicsmagick" "libwebp" "jpegoptim"
    "flatpak" "xdg-desktop-portal"
    "lshw" "strace" "nftables" "file" "lsof"))

(define %pkg-desktop
  '("pavucontrol" "dunst" "flameshot" "wireplumber"
    "lemonbar-xft" "xss-lock" "redshift" "unclutter" "playerctl"))

(define %pkg-fonts
  '("font-fira-code"
    "font-google-noto-emoji" ; 💪🕹
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
  '( ;; apps
    "qutebrowser"

    ;; "zeal" "qalculate-gtk" "simplescreenrecorder"
    ;; media
    "libreoffice" "qview" "stapler" "gimp"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu" "zathura-cb"
    "yt-dlp" "mpv"

    "fava" "beancount"
    ))

(define %pkg-dev
  '("python" "make"
    "git" "git:send-email" "git-crypt" "perl" ;for some git commands
    "libvirt" "virt-manager" "sicp" "openjdk:jdk" "clojure-tools"))

(define symlinks
  (resolve-relative-to "/storage/"
   '(("Sync")
     ("Resources")
     ("devel")
     ("devel/dotfiles" . ".dotfiles")
     ("Sync/pass" . ".password-store")
     ("data/telega" . ".telega")
     ("data/mail" . ".mail")
     ;; ("data/gnupg" . ".gnupg") ; TODO: clashes with home service
     ("data/events" . ".events")
     ("data/syncthing" . ".config/syncthing")
     ("data/qutebrowser" . ".local/share/qutebrowser")
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

         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (layout 'stow)
                   (directories '(".."))
                   (packages '("backup" "android" "email" "xsession" "git" "qutebrowser" "desktop"))))

         (simple-service 'configs
                         home-files-service-type
                         `((".config/mpv/scripts/mpris.so"
                            ,(file-append (specification->package "mpv-mpris") "/lib/mpris.so"))))

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
                           (url "https://github.com/sarg/dotfiles"))))

         (simple-service 'additional-env-vars-service
                         home-environment-variables-service-type
                         `(("PATH" . "$HOME/.local/bin:$PATH")
                           ("QT_PLUGIN_PATH" . ,(file-append qtwayland "/lib/qt6/plugins"))
                           ("QT_QPA_PLATFORM_PLUGIN_PATH" . ,(file-append qtwayland  "/lib/qt6/plugins/platforms"))
                           ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
                           ("BROWSER". "qutebrowser")))))))
