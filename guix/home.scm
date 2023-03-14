(use-modules
  (guix gexp)
  (guix modules)
  (guix utils)
  (guix channels)
  (gnu home)
  (gnu packages)
  (gnu home services)
  (gnu home services shepherd)
  (gnu home services desktop)
  (gnu home services guix)
  (gnu services)
  (gnu packages xorg)
  (gnu services xorg)
  (srfi srfi-11)
  (ice-9 match)
  (ice-9 ftw)
  (guix packages)
  (gnu packages xdisorg)
  (gnu home services shells))

(define %user "sarg")

(define %pkg-android
  '("adb" "fdroidcl" "socat" "scrcpy"))

(define %pkg-utils
  '("aria2" "curl" "rsync" "plocate"
    "atool" "p7zip" "unzip" "jq"
    "ripgrep" "moreutils" "libiconv"
    "powertop" "graphviz"
    "git" "git:send-email" "git-crypt"
    "bind:utils"                        ; dig
    "lshw" "strace" "nftables" "file"))

(define %pkg-desktop
  '("pavucontrol" "dunst" "flameshot"
    "pulseaudio" "dbus" "polybar"
    "redshift" "udiskie"))

(define %pkg-fonts
  '("font-fira-code"
    ;; "font-google-noto"
    "font-hack"
    "font-terminus"))

(define %pkg-x11
  '("picom" "xhost" "xkbcomp" "xkbset"
    "xprop" "xrandr" "xset" "xwininfo"
    "xev" "xclip" "xinput" "unclutter"
    "hicolor-icon-theme" "tango-icon-theme"
    "igt-gpu-tools"                     ; intel graphics tool
    ))

(define %pkg-games
  '(;; "lierolibre" "chroma" "meandmyshadow" "gcompris-qt"
    ;; "tipp10" "quakespasm" "sgt-puzzles" "xonotic"
    "quake3e"))

(define %pkg-apps
  '(          ;; apps
    "calibre" ;; "goldendict"
    "anki" "qutebrowser"
    "openvpn" "openssh"
    "mu" "msmtp" "isync"
    "gnupg" "pass-otp" "password-store" "pinentry-emacs" "pwgen"
    "piper"

    ;; "zeal" "qalculate-gtk" "simplescreenrecorder"
    ;; media
    "libreoffice" "qview" "stapler" "gimp" "imagemagick"
    "graphicsmagick" "libwebp"

    ;; "nomacs"
    "zathura" "zathura-pdf-mupdf" "zathura-djvu"
    "yt-dlp" "mpv" "readymedia"
    "jpegoptim"

    ;; dev
    ;; "openjdk:jdk"
    "python"))

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
                  (string-join (list "exec" (assoc-ref inputs "xwrapper")))))))))))))

(define minidlna-service
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
   (stop #~(make-kill-destructor))))

(define symlinks
  '(("Sync" "Sync")
    ("Resources" "Resources")
    ("devel" "devel")
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
           (lambda (src-tgt)
             (let ((source-file (string-append storage (car src-tgt)))
                   (target-file (string-append home (cadr src-tgt))))

               (unless (no-follow-file-exists? target-file)
                 (symlink source-file target-file))))

           '#$symlinks)))))

(define %emacs-home (load "./emacs-home.scm"))
(home-environment
 (packages
  (append (list my-sx)
          (home-environment-packages %emacs-home)
          (map (compose list specification->package+output)
               (append %pkg-android
                       %pkg-games
                       %pkg-utils
                       %pkg-desktop
                       %pkg-fonts
                       %pkg-x11
                       %pkg-apps))))

 (services
  (append
   ;; (home-environment-services %emacs-home)
   (list (service
          home-bash-service-type
          (home-bash-configuration
           (bash-profile
            (list (plain-file
                   "bash_profile"
                   "[[ ! $DISPLAY && $(tty) == /dev/tty1 ]] && exec sx sh ~/.xsession")))))

         (service home-dbus-service-type)
         (simple-service 'symlinks
                         home-activation-service-type
                         symlinks-activation)

         (simple-service 'configs
                         home-files-service-type
                         (append
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
