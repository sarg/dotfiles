(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix git-download)
             (guix packages)
             (guix channels)
             (guix inferior)
             (guix profiles)
             (guix download)
             (guix transformations)
             (guix utils)
             (srfi srfi-1)
             (ice-9 textual-ports)
             ;; (pkill9 fhs)
             (personal packages xdisorg)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-package-modules
 linux ssh android suckless dns
 xdisorg pulseaudio connman xorg gnome)

(use-service-modules
 desktop ssh networking sysctl
 xorg dbus shepherd sound pm)

(define %linux-5.10
  (let* ((channels
          (list (channel
                 (name 'nonguix)
                 (url "https://gitlab.com/nonguix/nonguix")
                 (commit "ef93718373b90cad8d25419e45ad4620403acded"))
                (channel
                 (name 'guix)
                 (url "https://git.savannah.gnu.org/git/guix.git")
                 (commit "02e5c95db958a434a42c83f19c7f65437776831e"))))
         (inferior
          (inferior-for-channels channels)))
    (first (lookup-inferior-packages inferior "linux" "5.10.9"))))

(define %grub-lubuntu-14 "
menuentry \"Lubuntu 14.04 ISO\" {
    set isofile=\"/home/sarg/Downloads/focal-desktop-amd64.iso\"
    loopback loop ($root)$isofile
    linux (loop)/casper/vmlinuz boot=casper iso-scan/filename=${isofile} quiet splash
    initrd (loop)/casper/initrd
}
")

(define dhclient-enter-hooks "
make_resolv_conf() {
    touch /etc/dnsmasq.servers
    sed -i \"/#dhcp/,+1d;
             $ a #dhcp\\nserver=/${new_domain_name}/${new_domain_name_servers}\\n\" \\
        /etc/dnsmasq.servers

    kill -HUP $(cat /run/dnsmasq.pid)
}
")

(define extrakeys-service-type
  (shepherd-service-type
   'extrakeys
   (lambda (codes)
     (shepherd-service
      (documentation (string-append "Load extra keys (setkeycodes)."))
      (provision '(extrakeys))
      (start #~(lambda _
                 (zero? (system* #$(file-append kbd "/bin/setkeycodes")
                                 #$@codes))))
      (respawn? #f)))
   (description "Map special keys")))

(define (append-to-computed-file g text)
  #~(begin
      #$g

      (let ((port (open-file #$output "a")))
        (format port #$text)
        (close port))))

(define libratbag-0.16
  ((options->transformation
    '((with-commit . "libratbag=v0.16")))
   libratbag))

(define-public (manifest->packages manifest) ;; copied from guix/scripts/refresh.scm, referring to it with (@@ ...) stopped working for some reason, kept saying it's an unbound variable
  "Return the list of packages in MANIFEST."
  (filter-map (lambda (entry)
                (let ((item (manifest-entry-item entry)))
                  (if (package? item) item #f)))
              (manifest-entries manifest)))


(define fhs-packages ;; list of packages to use for fhs-service
  (manifest->packages
   (specifications->manifest
    (list
     "libmediainfo" "libxscrnsaver" "eudev" "alsa-lib" "libzen"
     "fontconfig" "zlib" "pulseaudio" "libx11"
     "libnet" "libxcb" "libxau" "libxdmcp" "libxxf86vm" "libxext" "libxi"
     "libxrandr" "libxrender" "libxcursor" "libxfixes" "libxinerama" "libxcb"
     "glu" "libdrm" "libxdamage" "expat" "libpciaccess"

     ;; "libxcomposite" "libxtst" "libxaw" "libxt" "libxrandr" "libxext" "libx11"
     ;; "libxfixes" "glib" "gtk+" "gtk+@2" "bzip2" "zlib" "gdk-pixbuf" "libxinerama"
     ;; "libxdamage" "libxcursor" "libxrender" "libxscrnsaver" "libxxf86vm"
     ;; "libxi" "libsm" "libice" "gconf" "freetype" "curl" "nspr" "nss" "fontconfig"
     ;; "cairo" "pango" "expat" "dbus" "cups" "libcap" "sdl2" "libusb" "dbus-glib"
     ;; "atk" "eudev" "network-manager" "pulseaudio" "openal" "alsa-lib" "mesa"
     ;; "libxmu" "libxcb" "glu" "util-linux" "libogg" "libvorbis" "sdl" "sdl2-image"
     ;; "glew" "openssl" "libidn" "tbb" "flac" "freeglut" "libjpeg" "libpng" "libpng@1.2"
     ;; "libsamplerate" "libmikmod" "libtheora" "libtiff" "pixman" "speex" "sdl-image"
     ;; "sdl-ttf" "sdl-mixer" "sdl2-ttf" "sdl2-mixer" "gstreamer" "gst-plugins-base"
     ;; "glu" "libcaca" "libcanberra" "libgcrypt" "libvpx"
     ;;"librsvg" ;; currently requires compiling, but shouldn't, it's being weird
     ;; "libxft" "libvdpau" "gst-plugins-ugly" "libdrm" "xkeyboard-config" "libpciaccess"
     ;;"ffmpeg@3.4" ;; Disable this because test fails at livf-something (373grdi4fc369v2h29g10672whmv0mvb-ffmpeg-3.4.7.drv)
     ;; "libpng" "libgpg-error" "sqlite" "libnotify"

     ;; "fuse" "e2fsprogs" "p11-kit" "xz" "keyutils" "xcb-util-keysyms" "libselinux"
     ;; "ncurses" "jack" "jack2" "vulkan-loader" "at-spi2-atk" "at-spi2-core" "libsigc++"
     ))))



(define* (grub-conf-with-custom-part fn)
  ;; fn returns computed-file
  (lambda* (#:rest r)
    (let ((grubcfg-computed-file (apply fn r)))
      (computed-file
       (computed-file-name grubcfg-computed-file)
       (append-to-computed-file
        (computed-file-gexp grubcfg-computed-file)
        (string-append
         %grub-lubuntu-14))
       #:options (computed-file-options grubcfg-computed-file)
       ;; #:options '(#:local-build? #t #:substitutable? #f)
       ))))

(define dnsmasq-service-type
  (shepherd-service-type
   'dnsmasq
   (lambda (a)
     (shepherd-service
      (provision '(dnsmasq))
      (requirement '(networking))
      (documentation "Run the dnsmasq DNS server.")
      (start #~(make-forkexec-constructor
                '(#$(file-append dnsmasq "/sbin/dnsmasq")
                  "--keep-in-foreground"
                  "--pid-file=/run/dnsmasq.pid"
                  "--no-hosts"
                  "--local-service"
                  ;; "--enable-dbus"
                  ;; "--addn-hosts=/etc/hosts.coldturkey"
                  "--address=/dev.local/127.0.0.1"
                  "--address=/local/127.0.0.1"
                  "--servers-file=/etc/dnsmasq.servers"
                  "--no-resolv"
                  "--server=1.1.1.1")
                #:pid-file "/run/dnsmasq.pid"))
      (stop #~(make-kill-destructor))))
   (description "Dnsmasq service")))

(define wifi-udev-rule
  (udev-rule
   "80-wifi.rules"
   "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"iwlwifi\", KERNEL==\"wl*\", NAME=\"wifi\""))

(operating-system
  (kernel %linux-5.10)
  (kernel-arguments '("quiet" "loglevel=1" "ipv6.disable=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware broadcom-bt-firmware %base-firmware))
  (locale "en_GB.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us"))
  ;; (bootloader
  ;;  (bootloader-configuration
  ;;   (bootloader (bootloader
  ;;                (inherit grub-bootloader)
  ;;                (configuration-file-generator
  ;;                 (grub-conf-with-custom-part
  ;;                  (bootloader-configuration-file-generator grub-bootloader)))))
  ;;   (targets '("/dev/sda"))
  ;;   (keyboard-layout keyboard-layout)))
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot"))))
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device (uuid "b2c8548b-de1a-4d6c-8ada-2a60dc50e41b" 'ext4))
            (type "ext4"))
          (file-system
           (mount-point "/boot")
           (device (uuid "298B-1EF1" 'fat32))
           (type "vfat"))
          (file-system
           (mount-point "/media/sarg/500GB")
           (device (uuid "8ae97db9-a22c-4476-b7ad-2e82646f5fec" 'ext4))
           (type "ext4"))
          %base-file-systems))
  (host-name "thinkpad")

  (groups (cons* (user-group
                  (system? #t)
                  (name "adbusers"))
                 %base-groups))

  (users (cons* (user-account
                 (name "sarg")
                 (comment "Sergey Trofimov")
                 (group "users")
                 (home-directory "/home/sarg")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "tty" "input" "adbusers" "kvm")))
                %base-user-accounts))

  (packages
   (cons*
    (specification->package "nss-certs")
    (specification->package "bluez")
    brightnessctl
    tlp
    libratbag-0.16
    (filter (lambda (p)
              (not (member (package-name p)
                           '("wireless-tools" "info-reader" "nano" "zile"))))
            %base-packages)))

  (services (cons*
             ;; (service connman-service-type)
             (service wpa-supplicant-service-type
                      (wpa-supplicant-configuration
                       (interface "wifi")
                       (config-file (mixed-text-file
                                     "wpa_supplicant.conf"
                                     "ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev\n"
                                     "update_config=0\n"
                                     (call-with-input-file
                                         (string-append (current-source-directory) "/../secure/wifi_connections")
                                       get-string-all)))
                       (extra-options '("-Dnl80211"))))

             (service dhcp-client-service-type)
             (extra-special-file "/etc/dhclient-enter-hooks"
                                 (plain-file "dhclient-enter-hooks"
                                             dhclient-enter-hooks))
             (extra-special-file "/etc/dhclient.conf"
                                 (plain-file "dhclient.conf"
                                             "send host-name = gethostname();"))

             (screen-locker-service physlock)
             ;; (screen-locker-service xlockmore "xlock")

             ;; Add polkit rules, so that non-root users in the wheel group can
             ;; perform administrative tasks (similar to "sudo").
             polkit-wheel-service
             (bluetooth-service)

             (udisks-service)
             (service polkit-service-type)
             (service elogind-service-type
                      (elogind-configuration
                       (handle-lid-switch-external-power 'suspend)))


             ;; (service fhs-binaries-compatibility-service-type
             ;;          (fhs-configuration
             ;;           (lib-packages fhs-packages)))

             (dbus-service)
             (simple-service 'ratbagd dbus-root-service-type `(,libratbag-0.16))
             x11-socket-directory-service
             (service tlp-service-type
                      (tlp-configuration
                       (restore-device-state-on-startup? #t)))
             (service pulseaudio-service-type
                      (pulseaudio-configuration
                       (daemon-conf '((flat-volumes . no)
                                      (exit-idle-time . -1)))))
             (service alsa-service-type)

             (service openssh-service-type
                      (openssh-configuration
                       (x11-forwarding? #t)))
             (service dnsmasq-service-type 0)

             ;; remap lctrl->lalt, lalt->lctrl, capslock->lshift
             (service extrakeys-service-type (list "1d" "56" "38" "29" "3a" "42"))

             (modify-services %base-services
                (sysctl-service-type config =>
                        (sysctl-configuration
                         (inherit config)
                         (settings (append '(("fs.inotify.max_user_watches" . "524288")
                                             ("net.ipv6.conf.all.disable_ipv6" . "1"))
                                           (sysctl-configuration-settings config)))))

               (udev-service-type
                config =>
                (udev-configuration
                 (inherit config)
                 (rules (cons*
                         brightnessctl
                         android-udev-rules
                         wifi-udev-rule
                         (udev-configuration-rules config)))))))))
