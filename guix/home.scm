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
 (gnu home services pm)
 (gnu home services sound)
 (gnu home services shells)
 (gnu home services guix)
 (personal services symlinks)
 (personal services supercron)
 (personal packages binary)
 (srfi srfi-1)
 (srfi srfi-11))

(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync"
    "atool" "p7zip" "unzip" "jq" "openssh"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz" "bind:utils"  ; dig
    "graphicsmagick" "libwebp" "jpegoptim"
    "flatpak" "xdg-desktop-portal-gtk"
    "lshw" "strace" "nftables" "file" "lsof"))

(define %pkg-desktop
  '("pavucontrol" "flameshot" "wireplumber" "playerctl"))

(define %pkg-fonts
  '("font-fira-code"
    "font-google-noto-emoji" ; 💪🕹
    "font-hack"
    "font-terminus"))

(define %pkg-x11
  '("xkbcomp" "xkbset" "xkb-switch"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput"
    "hicolor-icon-theme" "adwaita-icon-theme"
    "greybird-gtk-theme"))

(define %pkg-games
  '(;; "lierolibre" "chroma" "meandmyshadow" "gcompris-qt"
    ;; "tipp10" "quakespasm" "sgt-puzzles" "xonotic"
    "quake3e"))

(define %pkg-apps
  '("qutebrowser"

    ;; "zeal" "qalculate-gtk" "simplescreenrecorder"
    "libreoffice" "qview" "stapler" "gimp-next" "calibre"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu" "zathura-cb"
    "yt-dlp" "mpv"
    ))

(define %pkg-dev
  '("python" "make" "fossil"
    "git" "git:send-email" "git-crypt" "perl"))

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
     ("data/stardict" . ".local/share/stardict")
     ("data/qutebrowser" . ".local/share/qutebrowser")
     ("apps/quake3" . ".q3a"))))

(define (x-autostart-on tty)
  (simple-service
   'x-autostart home-shell-profile-service-type
   (list (mixed-text-file
          "x-autostart"
          "[[ ! $DISPLAY && $(tty) == /dev/" tty " ]] && "
          "exec startx"
          ))))

(define (changelog-task fn)
  #~(make <task>
      #:name #$(string-append "changelog-" (basename fn))
      #:environment '(#$(string-append "PATH=" (getenv "HOME") "/.config/guix/current/bin")
                      "PWD=/storage/Resources/dashboard")
      #:schedule (list (make <interval>
                         #:start (time "2025-01-01T06:00:00+0000")
                         #:period (period "1d")))
      #:arguments '("/storage/Resources/dashboard/git2rss.clj" #$fn)))

(define %storage-backup-task
  #~(make <task>
      #:name "backup-storage"
      #:environment '(#$(string-append "HOME=" (getenv "HOME")))
      #:schedule (list (make <interval>
                         #:start (time "2025-01-01T06:00:00+0000")
                         #:period (period "1d")))
      #:arguments '(#$(file-append restic "/bin/restic")
                    "-p" "/media/500GB/restic/pass"
                    "-r" "/media/500GB/restic"
                    "--exclude-if-present" ".borgbackupexclude"
                    "backup" "/storage")))

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
   %base-home-services
   (home-environment-user-services %emacs-home)
   (list (service home-bash-service-type)

         (x-autostart-on "tty1")

         (service home-pipewire-service-type)
         (service home-symlinks-service-type symlinks)

         (service home-syncthing-service-type
                  (let ((pixel (syncthing-device (id "Q4ZQAU5-ZBFVS3E-OULHHHM-3HOCUXT-TVV5UYL-XPZRRWH-EXYYJWG-WVAUAAS")))
                        (thinkpad (syncthing-device (id "NYWEUMS-WOSRVEG-TD6CQZA-IHZ66GX-HZT2PJ2-IZ244FL-N3JC7DD-3DV57AG"))))

                    (for-home (syncthing-configuration
                               (config-file
                                (syncthing-config-file
                                 (ur-accepted -1)
                                 (folders (list (syncthing-folder
                                                 (label "Sync")
                                                 (path "~/Sync")
                                                 (devices (list pixel thinkpad)))))))))))
         (service home-dbus-service-type)
         (service home-batsignal-service-type)

         (service home-dotfiles-service-type
                  (home-dotfiles-configuration
                   (layout 'stow)
                   (directories '(".."))
                   (packages '("android" "email" "xsession" "git" "qutebrowser" "desktop"))))

         (simple-service 'changelog-jobs
          supercron-service-type
          (list
           (changelog-task "/storage/Resources/dashboard/guix.atom")
           (changelog-task "/storage/Resources/dashboard/doomemacs.atom")
           %storage-backup-task))

         (simple-service 'x11-configs
                         home-files-service-type
                         `((".icons/default"
                            ,(file-append (specification->package "bibata-cursor-theme")
                                          "/share/icons/Bibata-Modern-Ice"))
                           (".xinitrc"
                            ,(mixed-text-file "xinitrc"
                              "slock &\n"
                              (specification->package "xss-lock") "/bin/xss-lock -l lock.sh &\n"

                              ;; unredir fixes performance in fullscreen apps, e.g. q3
                              ;; https://github.com/chjj/compton/wiki/perf-guide
                              (specification->package "picom")
                              "/bin/picom --backend glx --vsync -b --unredir-if-possible\n"

                              "keymap.sh\n"

                              (specification->package "xhost")
                              "/bin/xhost +si:localuser:$USER\n"

                              "dbus-update-activation-environment --verbose DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY\n"

                              "emacs --init-directory=" (specification->package "doomemacs") " --eval '(exwm-enable)'\n"))))

         (simple-service 'configs
                         home-xdg-configuration-files-service-type
                         `(("mpv/scripts/mpris.so"
                            ,(file-append (specification->package "mpv-mpris") "/lib/mpris.so"))
                           ("mpv/fonts" ,(file-append (specification->package "mpv-uosc") "/share/mpv/fonts"))
                           ("mpv/scripts/thumbfast.lua"
                            ,(file-append (specification->package "mpv-thumbfast") "/share/mpv/scripts/thumbfast.lua"))
                           ("mpv/script-opts/thumbfast.conf"
                            ,(mixed-text-file "thumbfast.conf" "network=yes"))
                           ("mpv/scripts/uosc"
                            ,(file-append (specification->package "mpv-uosc") "/share/mpv/scripts/uosc"))))

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
                           ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                           ("QT_QPA_PLATFORMTHEME" . "gtk3")
                           ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
                           ("BROWSER" . "qutebrowser")))))))
