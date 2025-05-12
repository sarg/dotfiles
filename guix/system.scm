(use-modules (gnu)
             (gnu artwork)
             (gnu services)
             (gnu bootloader grub)
             (gnu system privilege)
             (gnu packages)
             (guix packages)
             (guix channels)
             (guix utils)
             (srfi srfi-1)
             (sops secrets)
             (sops services sops)
             (personal services hetzner)
             (personal services utils)
             (personal services iwd)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (personal packages udev-rules)
             (ice-9 textual-ports))

(use-package-modules
 linux ssh android search fonts firmware
 gnome admin cups spice xdisorg)

(use-service-modules
 admin desktop ssh networking sysctl cups avahi guix vpn
 xorg linux dbus shepherd sound pm dns virtualization)

(define (relative-file file)
  (string-append (current-source-directory) "/" file))

(define polkit-udisks-wheel
  (file-union
   "polkit-udisks-wheel"
   `(("share/polkit-1/rules.d/udisks-wheel.rules"
      ,(local-file "./files/udisks-wheel.rules")))))

(define %non-guix.pub
  (plain-file
   "non-guix.pub"
   "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))

(define (auto-login-to-tty config tty user)
  (if (string=? tty (mingetty-configuration-tty config))
      (mingetty-configuration
       (inherit config)
       (auto-login user)
       (shepherd-requirement
        (cons 'dbus-system
              (mingetty-configuration-shepherd-requirement
               config))))
      config))

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
    (theme (grub-theme
            (image (file-append %artwork-repository "/grub/guix-16-9.svg"))
            (resolution '(1600 . 900))
            (gfxmode '("1600x900" "auto"))))
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
            (mount-point "/media/500GB")
            (device (file-system-label "500GB"))
            (create-mount-point? #t)
            (mount-may-fail? #t)
            (type "ext4"))
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
         '("bluez" "intel-vaapi-driver" "restic"
           "tlp" "brightnessctl" "libratbag"))))

  (privileged-programs
   (append (list
            (privileged-program
             (program (file-append xsecurelock "/libexec/xsecurelock/authproto_pam"))
             (setuid? #t))

            (privileged-program
             (program (file-append spice-gtk "/libexec/spice-client-glib-usb-acl-helper"))
             (setuid? #t)))
           %default-privileged-programs))

  (services
   (append
    (modify-services %base-services
      (mingetty-service-type config => (auto-login-to-tty config "tty1" "sarg"))
      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          ;; prevent guix gc deleting build inputs
                          (privileged? #f)
                          (extra-options '("--gc-keep-derivations"
                                           "--gc-keep-outputs")))))

    (list
     (service pam-limits-service-type
              ;; For Lutris / Wine esync
              (list (pam-limits-entry "*" 'hard 'nofile 524288)))

     (service wireguard-service-type
              (wireguard-configuration
               (auto-start? #f)
               (shepherd-requirement '(sops-secrets))
               (addresses '("10.66.66.2/32" "fd42:42:42::2/128"))
               (private-key "/run/secrets/hetzner/wireguard/private")
               (peers
                (list
                 (wireguard-peer
                  (name "hetzner")
                  (endpoint "sargvpn.mooo.com:52817")
                  (public-key "6gNRvmvi5oRGSPr8J0dBcyDyKS94zO4Y4Jbwo2u+iV0=")
                  (preshared-key "/run/secrets/hetzner/wireguard/psk")
                  (allowed-ips '("0.0.0.0/0" "::/0")))))))

     (simple-service 'sysctl-custom sysctl-service-type
                     '(("fs.inotify.max_user_watches" . "524288")))

     (service fstrim-service-type)
     (service file-database-service-type
              (file-database-configuration
               (schedule #~(calendar-event #:hours '(6) #:minutes '(0)))
               (excluded-directories
                (append '("/home") %default-file-database-excluded-directories))))

     (service iwd-service-type)
     (service libvirt-service-type)
     (service virtlog-service-type)
     (service ntp-service-type)
     (service cups-service-type
              (cups-configuration
               (web-interface? #t)
               (extensions (list cups-filters))))

     (simple-service
      'resolvconf etc-service-type
      (list `("resolvconf.conf"
              ,(plain-file "resolvconf.conf"
                           (string-join '("name_servers=127.0.1.1"
                                          "resolv_conf_local_only=no"
                                          "dnsmasq=true"
                                          "dnsmasq_conf=/etc/dnsmasq.servers"
                                          "dnsmasq_resolve=/etc/dnsmasq.resolve")
                                        "\n")))))

     ;; Add polkit rules, so that non-root users in the wheel group can
     ;; perform administrative tasks (similar to "sudo").
     polkit-wheel-service
     (simple-service 'polkit-wheel-policies polkit-service-type
                     (list polkit-udisks-wheel spice-gtk))
     (service polkit-service-type)

     (service sops-secrets-service-type
              (sops-service-configuration
               (age-key-file "/root/agekeys.txt")
               (config (local-file (relative-file "../.sops.yaml") "sops.yaml"))
               (secrets (list
                         (sops-secret
                          (key '("hetzner" "wireguard" "psk"))
                          (file (local-file (relative-file "sops/hetzner.yaml")))
                          (output-type "binary"))

                         (sops-secret
                          (key '("hetzner" "wireguard" "private"))
                          (file (local-file (relative-file "sops/hetzner.yaml")))
                          (output-type "binary"))

                         (sops-secret
                          (key '("hetzner" "user-data" "vpn"))
                          (file (local-file (relative-file "sops/hetzner.yaml")))
                          (output-type "yaml"))

                         (sops-secret
                          (key '("hetzner" "hcloud.toml"))
                          (file (local-file (relative-file "sops/hetzner.yaml")))
                          (output-type "binary"))))))

     (service hetzner-vm-service-type
              (hetzner-vm
               (name "vpn")
               (type "cax11")
               (image "ubuntu-24.04")
               (user-data-file "/run/secrets/hetzner/user-data/vpn")
               (cli-config "/run/secrets/hetzner/hcloud.toml")
               (extra-args '("--location" "hel1"
                             "--primary-ipv6" "vpn_ipv6"
                             "--ssh-key" "thinkpad"))))

     (service upower-service-type)
     (service bluetooth-service-type)
     (service udisks-service-type)

     (service elogind-service-type
              (elogind-configuration
               (handle-lid-switch-external-power 'suspend)
               (kill-user-processes? #t)))
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
               (servers-file "/etc/dnsmasq.servers")
               (listen-addresses '("::1" "127.0.1.1"))
               ;; (addresses '("/dev.local/127.0.0.1"
               ;;              "/local/127.0.0.1"))
               (servers '("9.9.9.9"))
               (extra-options '("--bind-interfaces"
                                "--interface=lo"))))

     (simple-service
      'setkeycodes activation-service-type
      #~(invoke (string-append #$kbd "/bin/setkeycodes")
                "1d" "56"                  ; lctrl->lalt
                "38" "29"                  ; lalt->lctrl
                "3a" "42"))      ; capslock->lshift

     (udev-rules-service 'android android-udev-rules #:groups '("adbusers"))
     (udev-rules-service 'qmk qmk-udev-rules)
     (udev-rules-service 'kindle kindle-usbnet-udev-rules)
     (udev-rules-service 'pixel pixel-usbnet-udev-rules)
     (udev-rules-service 'brightness brightnessctl)
     (udev-rules-service 'restore-brightness backlight-udev-rules)
     (udev-rules-service 'leds iwlwifi-led-udev-rules)

     (simple-service
      'nongux-substitutes guix-service-type
      (guix-extension
       (substitute-urls '("https://substitutes.nonguix.org"))
       (authorized-keys (list %non-guix.pub))))))))
