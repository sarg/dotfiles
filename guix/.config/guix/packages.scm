(specifications->manifest
 '("adb" "fastboot" "fdroidcl"
   "socat" "scrcpy"

   ;; utils
   "aria2" "curl" "rsync"
   "atool" "p7zip" "unzip" "jq"
   "ripgrep" "stow" "moreutils"
   "powertop" "graphviz"
   "git" "git:send-email"

   ;; DE
   "brightnessctl" "pavucontrol"
   "physlock" "slock" "dunst" "flameshot"
   "pulseaudio" "dbus" "polybar" "redshift" "st"
   "awesome" "wpa-supplicant-gui"

   ;; counsel-linux-app uses gtk-launch
   "gtk+:bin"               ; for gtk-launch
   "glib:bin"               ; for gio-launch-desktop which is used by gtk-launch

   ;; fonts
   "font-fira-code"
   "font-google-noto"
   "font-hack"
   "font-terminus"

   ;; emacs
   "emacs-emacsql"
   "emacs-guix"
   "emacs-next"
   "emacs-pdf-tools"
   "emacs-telega"
   "emacs-vterm"

   "glibc-utf8-locales"

   ;; apps
   "calibre" "goldendict" "mpv" "qutebrowser"
   "syncthing" "udiskie" "youtube-dl"
   "zathura" "zathura-pdf-mupdf"
   "openvpn" "nomacs" "mu" "msmtp" "openssh" "isync"
   "gnupg" "pass-otp" "password-store" "pinentry-tty" "pwgen"
   "beancount" "anki" "readymedia" "piper"

   ;; media
   "libreoffice" "twinkle" "qview" "stapler"
   "jpegoptim"

   ;; dev
   "openjdk" "python"
   "lshw" "strace" "nftables" "file"

   ;; games
   "lierolibre" "chroma" "meandmyshadow" "gcompris-qt" "tipp10" "xonotic" "quake3e" "quakespasm"

   ;; x11
   "xf86-input-libinput" "xf86-video-intel" "picom"
   "xhost" "xinit" "xkbcomp" "xkbset" "xorg-server"
   "xprop" "xrandr" "xset" "xwininfo"))
