(use-modules (gnu)
             (gnu services)
             (gnu services)
             (gnu system setuid)
             (gnu packages)
             (guix packages)
             (guix channels)
             (guix utils)
             (srfi srfi-1)
             (personal services utils)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (ice-9 textual-ports))

(use-package-modules
 linux ssh android suckless fonts firmware
 xorg gnome admin cups spice)

(use-service-modules
 desktop ssh networking sysctl cups avahi guix vpn
 xorg dbus shepherd sound pm dns virtualization)

(define (relative-file file)
  (string-append (current-source-directory) "/" file))

(define extrakeys-service-type
  (shepherd-service-type
   'extrakeys
   (lambda (codes)
     (let ((setkeycodes (file-append kbd "/bin/setkeycodes"))
           (args (append-map (lambda (a) (list (car a) (cdr a))) codes)))
       (shepherd-service
        (documentation "Load extra keys (setkeycodes) at boot.")
        (provision '(extrakeys))
        (start #~(lambda _ (invoke #$setkeycodes #$@args)))
        (one-shot? #t))))
   (description "Map special keys")))

(define wifi-udev-rule
  (udev-rule
   "80-wifi.rules"
   "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"iwlwifi\", KERNEL==\"wl*\", NAME=\"wifi\""))

(define polkit-udisks-wheel
  (file-union
   "polkit-udisks-wheel"
   `(("share/polkit-1/rules.d/udisks-wheel.rules"
      ,(local-file "./files/udisks-wheel.rules")))))

(define polkit-udisks-wheel-service
  (simple-service 'polkit-udisks-wheel polkit-service-type (list polkit-udisks-wheel)))

(define polkit-spice-gtk-service
  (simple-service 'polkit-spice-gtk polkit-service-type (list spice-gtk)))

(define %non-guix.pub
  (plain-file
   "non-guix.pub"
   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(operating-system
  (kernel linux)
  (kernel-arguments '("quiet" "loglevel=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware broadcom-bt-firmware %base-firmware))
  (locale "en_GB.UTF-8")
  (timezone "Europe/Berlin")
  (host-name "thinkpad")
  (keyboard-layout (keyboard-layout "us"))
  (name-service-switch %mdns-host-lookup-nss)
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot"))))

  (file-systems
   (cons* (file-system
            (mount-point "/")
            (device (file-system-label "Guix_image"))
            (type "ext4"))
          (file-system
            (mount-point "/boot")
            (device (file-system-label "GNU-ESP"))
            (type "vfat"))
          %base-file-systems))

  (users (cons* (user-account
                 (name "sarg")
                 (comment "Sergey Trofimov")
                 (group "users")
                 (home-directory "/home/sarg")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "tty" "input" "adbusers" "kvm" "dialout" "cdrom" "libvirt")))
                %base-user-accounts))

  (packages
   (append
    (filter (lambda (p)
              (not (member (package-name p)
                           '("wireless-tools" "info-reader" "nano" "mg" "isc-dhcp"))))
            %base-packages)

    (map specification->package
         '("bluez" "intel-vaapi-driver"
           "tlp" "brightnessctl" "libratbag"))))

  (setuid-programs
   (cons* (setuid-program
           (program
            (file-append spice-gtk "/libexec/spice-client-glib-usb-acl-helper")))
          %setuid-programs))

  (services
   (append
    (modify-services %base-services
      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          ;; prevent guix gc deleting build inputs
                          (extra-options '("--gc-keep-derivations"
                                           "--gc-keep-outputs")))))

    (list
     (service guix-home-service-type
              `(("sarg" ,(load "./home.scm"))))

     (service pam-limits-service-type
              ;; For Lutris / Wine esync
              (list (pam-limits-entry "*" 'hard 'nofile 524288)))

     (no-autostart
      (service wireguard-service-type
               (wireguard-configuration
                (addresses '("10.66.66.2/32" "fd42:42:42::2/128"))
                (private-key "/home/sarg/.dotfiles/secure/wireguard.key")
                (peers
                 (list
                  (wireguard-peer
                   (name "hetzner")
                   (endpoint "[2a01:4f9:c012:f933::1]:52817")
                   (public-key "6gNRvmvi5oRGSPr8J0dBcyDyKS94zO4Y4Jbwo2u+iV0=")
                   (preshared-key "/home/sarg/.dotfiles/secure/wireguard.psk")
                   (allowed-ips '("0.0.0.0/0" "::/0"))))))))

     (simple-service 'sysctl-custom sysctl-service-type
                     '(("fs.inotify.max_user_watches" . "524288")))

     (service libvirt-service-type)
     (service virtlog-service-type)
     (service ntp-service-type)
     (service cups-service-type
              (cups-configuration
               (web-interface? #t)
               (extensions (list cups-filters))))

     (service wpa-supplicant-service-type
              (wpa-supplicant-configuration
               (interface "wifi")
               (config-file (mixed-text-file
                             "wpa_supplicant.conf"
                             "ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev\n"
                             "update_config=0\n"
                             (call-with-input-file (relative-file "../secure/wifi_connections")
                               get-string-all)))
               (extra-options '("-Dnl80211"))))

     (service xorg-server-service-type
              (xorg-configuration
               (modules (list xf86-video-intel xf86-input-libinput))
               (drivers (list "intel"))))

     (service dhcp-client-service-type
              (dhcp-client-configuration
               (interfaces '("wifi"))
               (config-file (plain-file "dhclient.conf" "send host-name = gethostname();"))
               (shepherd-requirement '(wpa-supplicant))))


     (simple-service 'dhclient-wan etc-service-type
                     (list `("resolvconf.conf"
                             ,(plain-file "resolvconf.conf"
                                          "name_servers=127.0.1.1\ndnsmasq_conf=/etc/dnsmasq.servers"))))

     (simple-service 'dhclient-wan etc-service-type
                     (list `("dhclient-enter-hooks"
                             ,(local-file "./files/dhclient-enter-hooks"))))

     (service screen-locker-service-type
              (screen-locker-configuration
               (name "slock")
               (program (file-append slock "/bin/slock"))))
     ;; Add polkit rules, so that non-root users in the wheel group can
     ;; perform administrative tasks (similar to "sudo").
     polkit-wheel-service
     polkit-udisks-wheel-service
     polkit-spice-gtk-service
     (service polkit-service-type)

     (service upower-service-type)
     (service bluetooth-service-type)
     (service udisks-service-type)

     (service elogind-service-type
              (elogind-configuration
               (handle-lid-switch-external-power 'suspend)))
     (service dbus-root-service-type)
     (simple-service 'ratbagd dbus-root-service-type (list libratbag))
     (service tlp-service-type
              (tlp-configuration
               (restore-device-state-on-startup? #t)))

     (service avahi-service-type)
     (service openssh-service-type
              (openssh-configuration
               (x11-forwarding? #t)))

     (service dnsmasq-service-type
              (dnsmasq-configuration
               (no-hosts? #t)
               (no-resolv? #t)
               (servers-file "/etc/dnsmasq.servers")
               (listen-addresses '("::1" "127.0.1.1"))
               ;; (addresses '("/dev.local/127.0.0.1"
               ;;              "/local/127.0.0.1"))
               (servers '("1.1.1.1"))
               (extra-options '("--bind-interfaces"
                                "--interface=lo"))))

     (service extrakeys-service-type
              '(("1d" . "56")   ; lctrl->lalt
                ("38" . "29")   ; lalt->lctrl
                ("3a" . "42"))) ; capslock->lshift

     (udev-rules-service 'android android-udev-rules #:groups '("adbusers"))
     (udev-rules-service 'wifi wifi-udev-rule)
     (udev-rules-service 'qmk qmk-udev-rules)
     (udev-rules-service 'brightness brightnessctl)

     (simple-service
      'nongux-substitutes guix-service-type
      (guix-extension
       (substitute-urls '("https://substitutes.nonguix.org"))
       (authorized-keys (list %non-guix.pub))))))))
