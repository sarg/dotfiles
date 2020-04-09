(specifications->manifest
 '("adb" "fastboot" "fdroidcl"
   "socat"

   ;; utils
   "aria2" "git" "curl" "rsync"
   "atool" "p7zip" "unzip" "jq"
   "ripgrep" "stow"
   "powertop" "strace" "graphviz"

   ;; DE
   "brightnessctl" "pavucontrol"
   "physlock" "slock" "dunst" "flameshot"
   "pulseaudio" "dbus"

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
   "emacs-next-snapshot"
   "emacs-pdf-tools"
   "emacs-telega"
   "emacs-vterm"

   "glibc-utf8-locales"

   ;; apps
   "calibre" "goldendict" "mpv" "qutebrowser"
   "syncthing" "udiskie" "youtube-dl"
   "zathura" "zathura-pdf-mupdf"
   "openvpn" "nomacs" "mu" "mbsync"
   "gnupg" "pass-otp" "password-store" "pinentry-tty"
   "redshift"

   ;; dev
   "openjdk"
   "python"

   ;; x11
   "xf86-input-libinput" "xf86-video-intel" "picom"
   "xhost" "xinit" "xkbcomp" "xkbset" "xorg-server" "xprop" "xrandr" "xset" "xwininfo"))
