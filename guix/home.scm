(use-modules
 (gnu)
 (gnu artwork)
 (guix)
 (guix gexp)
 (guix build utils)
 (guix utils)
 (guix modules)
 (guix channels)
 (gnu home)
 (gnu services)
 (gnu packages)
 (gnu packages admin)
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
 (gnu home services ssh)
 (gnu home services guix)
 (personal services symlinks)
 (personal services owntracks)
 (personal services screen-locker)
 (personal services utils)
 (personal services supercron)
 (personal services secretsd)
 (personal services backup)
 (personal packages binary)
 (personal packages next)
 (srfi srfi-1)
 (srfi srfi-11))

(define-syntax pkg (identifier-syntax specification->package))
(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync" "man-db" "atuin"
    "atool" "p7zip" "unzip" "jq" "openssh"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz" "bind:utils"  ; dig
    "graphicsmagick" "libwebp" "jpegoptim"
    "flatpak" "xdg-desktop-portal-gtk" "iwgtk"
    "lshw" "strace" "nftables" "file" "lsof"))

(define %pkg-desktop
  '("pavucontrol" "flameshot" "wireplumber" "playerctl"))

(define %pkg-fonts
  '("font-fira-code"
    "font-google-noto-emoji" ; 💪🕹
    "font-hack"
    "font-terminus"))

(define %pkg-x11
  '("xkbcomp" "xkbset" "xkb-switch" "setxkbmap"
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
    "libreoffice" "qview" "stapler" "gimp" "calibre"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu" "zathura-cb"
    "yt-dlp" "mpv" "tinymediamanager" "simple-scan-with-airscan"
    ))

(define %pkg-dev
  '("python" "make" "just"
    "git" "git:send-email" "git-crypt" "perl"))

(define symlinks
  (resolve-relative-to "/storage/"
   '(("Sync")
     ("Resources")
     ("devel")
     (".dotfiles"                 "devel/dotfiles")
     (".password-store"           "Sync/pass")
     (".telega"                   "data/telega")
     (".mail"                     "data/mail")
     (".gnupg"                    "data/gnupg/")
     (".events"                   "data/events")
     (".local/state/syncthing"    "data/syncthing/")
     (".local/share/stardict"     "data/stardict")
     (".local/share/qutebrowser"  "data/qutebrowser")
     (".q3a"                      "apps/quake3"))))

(define git2rss (load "../git2rss/guix.scm"))
(define (changelog-task fn)
  #~(make <task>
      #:name #$(string-append "changelog-" (basename fn))
      #:schedule (list (make <interval>
                         #:start (time "2025-01-01T06:00:00+0000")
                         #:period (period "1d")))
      #:arguments '(#$(file-append git2rss "/bin/git2rss") #$fn)))

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

         (simple-service
          'atuin-hook home-bash-service-type
          (home-bash-extension
           (aliases '(("g" . "just -g guix")))
           (bashrc (list
                    (mixed-text-file
                     "atuin-hook"
                     "source " (pkg "blesh") "/share/blesh/ble.sh\n"
                     "eval \"$(atuin init bash)\"")))))

         (service home-x11-service-type)
         (simple-service
          'startx
          home-shepherd-service-type
          (list
           (shepherd-service
            (provision '(xorg))
            (requirement '(dbus))
            (start #~(make-forkexec-constructor
                      (list #$startx)
                      #:create-session? #f))
            (stop #~(make-kill-destructor)))))

         (service home-shepherd-service-type
                  (home-shepherd-configuration
                   (auto-start? #f)
                   (daemonize? #f)))

         (simple-service
          'home-shepherd home-shell-profile-service-type
          (list (mixed-text-file
                 "home-shepherd"
                 "[[ $(tty) == /dev/tty1 ]] && exec "
                 shepherd-1.0 "/bin/shepherd"
                 " --silent"
                 " --config ~/.config/shepherd/init.scm")))

         (simple-service
          'extra-profile home-shell-profile-service-type
          (list (mixed-text-file
                 "extra-profile"
                 "GUIX_PROFILE=~/.guix-profile\n. $GUIX_PROFILE/etc/profile")))


         (service home-pipewire-service-type)

         (service mosquitto-service-type
                  (mixed-text-file "mosqiutto.conf"
                                   "listener 1883\n"
                                   "allow_anonymous true\n"))

         (service owntracks-service-type
                  (owntracks-service
                   (config (owntracks-configuration
                            (storage-dir "/storage/data/owntracks")))))

         (service home-openssh-service-type
                  (home-openssh-configuration
                   (hosts
                    (list
                     (openssh-host
                      (name "kindle")
                      (host-name "192.168.15.244")
                      (user "root"))
                     (openssh-host
                      (name "puxel")
                      (identity-file "~/.ssh/puxel_rsa")
                      (extra-content "IdentitiesOnly yes")
                      (port 8022))))))

         (service home-syncthing-service-type
                  (let ((pixel (syncthing-device (id "Q4ZQAU5-ZBFVS3E-OULHHHM-3HOCUXT-TVV5UYL-XPZRRWH-EXYYJWG-WVAUAAS")))
                        (thinkpad (syncthing-device (id "NYWEUMS-WOSRVEG-TD6CQZA-IHZ66GX-HZT2PJ2-IZ244FL-N3JC7DD-3DV57AG"))))

                    (for-home (syncthing-configuration
                               (config-file
                                (syncthing-config-file
                                 (usage-reporting-accepted -1)
                                 (gui-enabled? #f)
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
                   (packages '("android" "email" "xsession" "git" "qutebrowser" "desktop" "vim"))))

         (service restic-backup-service-type
                  (restic-backup-configuration
                   (jobs
                    (list
                     (restic-backup-job
                      (name "storage")
                      (repository "/media/500GB/restic")
                      (password-file "/media/500GB/restic/pass")
                      (schedule #~(list #:start (time "2025-01-01T06:00:00+0000")
                                        #:period (period "1d")))
                      (files (list "/storage"))
                      (extra-flags (list "--exclude-if-present=.borgbackupexclude")))))))

         (simple-service
          'changelog-jobs
          supercron-service-type
          (list
           (changelog-task "/storage/Resources/dashboard/guix.atom")
           (changelog-task "/storage/Resources/dashboard/doomemacs.atom")))

         (service secretsd-service-type
                  "exec:ssh puxel -- ./unlock-keyring")
         (simple-service
          'x11-configs
          home-files-service-type
          `(,@symlinks

            (".icons/default"
             ,(file-append (pkg "bibata-cursor-theme")
                           "/share/icons/Bibata-Modern-Ice"))

            (".xinitrc"
             ,(mixed-text-file "xinitrc"
                               (screen-locker (video-saver "/storage/data/splash.mp4")) " &\n"
                               "keymap.sh\n"
                               (pkg "xhost") "/bin/xhost +si:localuser:$USER\n"
                               "dbus-update-activation-environment --verbose DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY\n"

                               "while true; do\n"
                               "  xset s activate\n" ; lock to show password screen
                               "  emacs -mm --init-directory=" (pkg "doomemacs") " -f exwm-enable\n"
                               "done\n"))))

         (simple-service
          'configs
          home-xdg-configuration-files-service-type
          `(("guix/channels.scm" ,(local-file "channels.scm"))
            ("mpv/scripts/mpris.so"
             ,(file-append (pkg "mpv-mpris") "/lib/mpris.so"))
            ("mpv/fonts" ,(file-append (pkg "mpv-uosc") "/share/mpv/fonts"))
            ("mpv/scripts/thumbfast.lua"
             ,(file-append (pkg "mpv-thumbfast") "/share/mpv/scripts/thumbfast.lua"))
            ("mpv/script-opts/thumbfast.conf"
             ,(mixed-text-file "thumbfast.conf" "network=yes"))
            ("mpv/scripts/uosc"
             ,(file-append (pkg "mpv-uosc") "/share/mpv/scripts/uosc"))))

         (simple-service
          'additional-env-vars-service
          home-environment-variables-service-type
          `(("PATH" . "$HOME/.local/bin:$PATH")
            ("QT_PLUGIN_PATH" . ,(file-append qtwayland "/lib/qt6/plugins"))
            ("QT_QPA_PLATFORM_PLUGIN_PATH" . ,(file-append qtwayland  "/lib/qt6/plugins/platforms"))
            ("_JAVA_AWT_WM_NONREPARENTING" . "1")
            ("QT_QPA_PLATFORMTHEME" . "gtk3")
            ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
            ("BROWSER" . "qutebrowser")))))))
