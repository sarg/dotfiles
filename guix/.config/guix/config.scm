(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix packages)
             (guix profiles)
             (guix download)
             (guix utils)
             (srfi srfi-1)
             (pkill9 fhs)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-package-modules
 linux ssh android suckless dns
 xdisorg pulseaudio connman xorg)

(use-service-modules
 desktop ssh networking sysctl
 xorg dbus shepherd sound pm)

(define (corrupt-linux freedo version hash)
  (package
    (inherit freedo)
    (name "linux")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://www.kernel.org/pub/linux/kernel/v"
                                        (version-major version) ".x/linux-" version ".tar.xz")))
              (sha256 (base32 hash))))
    (home-page "https://www.kernel.org/")
    (synopsis "Linux kernel with nonfree binary blobs included")
    (description
     "The unmodified Linux kernel, including nonfree blobs, for running GuixSD
on hardware which requires nonfree software to function.")))

(define %linux-5.4.21
  (corrupt-linux linux-libre-5.4 "5.4.21"
                 "1yjv8qg47kb4j4jkcpi9z7v07p0vz3gszpmhrfji5866j97748vd"))

(define %my-syslog.conf
  ;; from http://www.linuxfromscratch.org/lfs/view/stable/chapter06/sysklogd.html
  (plain-file "syslog.conf" "
auth,authpriv.* -/var/log/auth.log
*.*;auth,authpriv.none -/var/log/sys.log
daemon.* -/var/log/daemon.log
kern.* -/var/log/kern.log
mail.* -/var/log/mail.log
user.* -/var/log/user.log
#*.emerg *
"))

(define %custom-grub-entry "
menuentry \"Ubuntu 14.04 ISO\" {
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
      (respawn? #f)))))

(define (append-to-computed-file g text)
  #~(begin
      #$g

      (let ((port (open-file #$output "a")))
        (format port #$text)
        (close port))))

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
        %custom-grub-entry)
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
                  "--address=/dev.local/127.0.0.1"
                  "--address=/local/127.0.0.1"
                  "--servers-file=/etc/dnsmasq.servers"
                  "--no-resolv"
                  "--server=1.1.1.1")
                #:pid-file "/run/dnsmasq.pid"))
      (stop #~(make-kill-destructor))))))

(define wifi-udev-rule
  (udev-rule
   "80-wifi.rules"
   "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"iwlwifi\", KERNEL==\"wl*\", NAME=\"wifi\""))

(operating-system
  (kernel %linux-5.4.21)
  (kernel-arguments '("quiet" "loglevel=1" "ipv6.disable=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware broadcom-bt-firmware %base-firmware))
  (locale "en_GB.utf8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us"))
  (bootloader
   (bootloader-configuration
    (bootloader (bootloader
                 (inherit grub-bootloader)
                 (configuration-file-generator
                  (grub-conf-with-custom-part
                   (bootloader-configuration-file-generator grub-bootloader)))))
    (target "/dev/sda")
    (keyboard-layout keyboard-layout)))
  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device
             (uuid "b2c8548b-de1a-4d6c-8ada-2a60dc50e41b"
                   'ext4))
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
                  '("wheel" "netdev" "audio" "video" "tty" "input" "adbusers")))
                %base-user-accounts))

  (packages
   (cons*
    (specification->package "nss-certs")
    (specification->package "bluez")
    ;; xorg-server
    ;; xf86-input-libinput xf86-video-intel
    brightnessctl
    tlp
    (filter (lambda (p)
              (not (member (package-name p)
                           '("wireless-tools" "info-reader" "nano" "zile"))))
            %base-packages)))

  (services (cons*
             ;; (service connman-service-type)
             (service wpa-supplicant-service-type
                      (wpa-supplicant-configuration
                       (interface "wifi")
                       (config-file "/etc/network/wpa-supplicant.conf")
                       (extra-options '("-Dnl80211"))))

             (service dhcp-client-service-type)
             (extra-special-file "/etc/dhclient-enter-hooks"
                                 (plain-file "dhclient-enter-hooks"
                                             dhclient-enter-hooks))
             (extra-special-file "/etc/dhclient.conf"
                                 (plain-file "dhclient.conf"
                                             "send host-name = gethostname();"))

             (screen-locker-service slock)
             ;; (screen-locker-service xlockmore "xlock")

             ;; Add polkit rules, so that non-root users in the wheel group can
             ;; perform administrative tasks (similar to "sudo").
             polkit-wheel-service
             (bluetooth-service)

             (udisks-service)
             (service polkit-service-type)
             (elogind-service)

             (service sysctl-service-type
                      (sysctl-configuration
                       (settings '(("kernel.printk" . "2 4 1 7")))))

             (service fhs-binaries-compatibility-service-type
                      (fhs-configuration
                       (lib-packages fhs-packages)))

             (dbus-service)
             x11-socket-directory-service
             (service tlp-service-type
                      (tlp-configuration
                       (restore-device-state-on-startup? #t)))
             (service pulseaudio-service-type
                      (pulseaudio-configuration
                       (daemon-conf '((flat-volumes . no)
                                      (exit-idle-time . -1)))))
             (service alsa-service-type)

             (service openssh-service-type)
             (service dnsmasq-service-type 0)

             ;; remap lctrl->lalt, lalt->lctrl, capslock->lshift
             (service extrakeys-service-type (list "1d" "56" "38" "29" "3a" "42"))

             (modify-services %base-services
               (syslog-service-type
                config =>
                (syslog-configuration
                 (inherit config)
                 (config-file %my-syslog.conf)))

               (udev-service-type
                config =>
                (udev-configuration
                 (inherit config)
                 (rules (cons*
                         brightnessctl
                         android-udev-rules
                         wifi-udev-rule
                         (udev-configuration-rules config)))))))))
