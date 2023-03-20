(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix packages)
             (guix channels)
             (guix utils)
             (srfi srfi-1)
             (personal packages xdisorg)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (ice-9 textual-ports))

(use-package-modules
 linux ssh android suckless
 pulseaudio connman xorg gnome admin)

(use-service-modules
 desktop ssh networking sysctl
 xorg dbus shepherd sound pm dns)

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

(define %non-guix.pub
  (plain-file
   "non-guix.pub"
   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(operating-system
  (kernel linux)
  (kernel-arguments '("quiet" "loglevel=1" "ipv6.disable=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware broadcom-bt-firmware %base-firmware))
  (locale "en_GB.UTF-8")
  (timezone "Europe/Berlin")
  (keyboard-layout (keyboard-layout "us"))
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
          (file-system
            (mount-point "/storage")
            (device (file-system-label "STORAGE"))
            (type "ext4"))
          %base-file-systems))
  (host-name "thinkpad")

  (users (cons* (user-account
                 (name "sarg")
                 (comment "Sergey Trofimov")
                 (group "users")
                 (home-directory "/home/sarg")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "tty" "input" "adbusers" "kvm" "dialout" "cdrom")))
                %base-user-accounts))

  (packages
   (append
    (filter (lambda (p)
              (not (member (package-name p)
                           '("wireless-tools" "info-reader" "nano" "mg"))))
            %base-packages)

    (map specification->package
         '("nss-certs" "bluez" "borgmatic"
           "tlp" "brightnessctl" "libratbag"))))

  (services (cons*
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
             (simple-service 'dhclient-wan etc-service-type
                             (list `("dhclient-enter-hooks"
                                     ,(local-file "./files/dhclient-enter-hooks"))
                                   `("dhclient.conf"
                                     ,(plain-file "dhclient.conf" "send host-name = gethostname();"))))

             (service screen-locker-service-type
                      (screen-locker-configuration
                       "physlock" (file-append physlock "/bin/physlock") #f))

             ;; Add polkit rules, so that non-root users in the wheel group can
             ;; perform administrative tasks (similar to "sudo").
             polkit-wheel-service
             polkit-udisks-wheel-service
             (service bluetooth-service-type)

             (service udisks-service-type)
             (service polkit-service-type)
             (service elogind-service-type
                      (elogind-configuration
                       (handle-lid-switch-external-power 'suspend)))

             (service dbus-root-service-type)
             (simple-service 'ratbagd dbus-root-service-type (list libratbag))
             (service tlp-service-type
                      (tlp-configuration
                       (restore-device-state-on-startup? #t)))
             (service pulseaudio-service-type
                      (pulseaudio-configuration
                       (daemon-conf '((flat-volumes . no)
                                      (default-sample-rate . 192000)
                                      (default-sample-format . s32le)
                                      (avoid-resampling . yes)
                                      (exit-idle-time . -1)))))
             (service alsa-service-type)

             (service openssh-service-type
                      (openssh-configuration
                       (x11-forwarding? #t)))
             (service dnsmasq-service-type
                      (dnsmasq-configuration
                       (no-hosts? #t)
                       (no-resolv? #t)
                       (servers-file "/etc/dnsmasq.servers")
                       (addresses '("/dev.local/127.0.0.1"
                                    "/local/127.0.0.1"))
                       (servers '("1.1.1.1"))))

             ;; remap lctrl->lalt, lalt->lctrl, capslock->lshift
             (service extrakeys-service-type (list "1d" "56" "38" "29" "3a" "42"))
             (udev-rules-service 'android android-udev-rules #:groups '("adbusers"))
             (udev-rules-service 'wifi wifi-udev-rule)
             (udev-rules-service 'brightness brightnessctl)

             (modify-services %base-services
               (sysctl-service-type config =>
                                    (sysctl-configuration
                                     (inherit config)
                                     (settings (append
                                                (sysctl-configuration-settings config)
                                                '(("fs.inotify.max_user_watches" . "524288")
                                                  ("net.ipv6.conf.all.disable_ipv6" . "1"))))))
               (guix-service-type config =>
                                  (guix-configuration
                                   (inherit config)
                                   (substitute-urls
                                    (append (guix-configuration-substitute-urls config)
                                            '("https://substitutes.nonguix.org")))
                                   (authorized-keys
                                    (append (guix-configuration-authorized-keys config)
                                            `(,%non-guix.pub)))))))))
