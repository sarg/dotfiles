(use-modules (gnu)
             (gnu services)
             (gnu services)
             (gnu system privilege)
             (gnu packages)
             (guix packages)
             (guix channels)
             (guix utils)
             (srfi srfi-1)
             (personal services utils)
             (personal services iwd)
             (nongnu system linux-initrd)
             (nongnu packages linux)
             (ice-9 textual-ports))

(use-package-modules
 linux ssh android search suckless fonts firmware
 xorg gnome admin cups spice)

(use-service-modules
 admin desktop ssh networking sysctl cups avahi guix vpn
 linux xorg dbus shepherd sound pm dns virtualization backup)

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

(define (auto-login-to-tty config tty user)
  (if (string=? tty (mingetty-configuration-tty config))
      (mingetty-configuration
       (inherit config)
       (login-pause? #t)
       (auto-login user))
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
         '("bluez" "intel-vaapi-driver"
           "tlp" "brightnessctl" "libratbag"))))

  (privileged-programs
   (append (list
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
                          (extra-options '("--gc-keep-derivations"
                                           "--gc-keep-outputs")))))

    (list
     ;; this is necessary only for guix image
     ;; otherwise home is updated more frequent than system
     ;; and it doesn't work because system loads
     ;; the built-in generation after reboot
     ;; (service guix-home-service-type
     ;;          `(("sarg" ,(load "./home.scm"))))

     (service pam-limits-service-type
              ;; For Lutris / Wine esync
              (list (pam-limits-entry "*" 'hard 'nofile 524288)))

     (service restic-backup-service-type
              (restic-backup-configuration
               (jobs (list
                      (restic-backup-job
                       (name "backup-storage")
                       (repository "/media/500GB/restic")
                       (requirement '(file-system-/media/500GB))
                       (password-file "/media/500GB/restic/pass")
                       (schedule #~(calendar-event #:hours '(0) #:minutes '(0)))
                       (files (list "/storage"))
                       (extra-flags (list "--exclude-if-present" ".borgbackupexclude")))))))

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

     (service fstrim-service-type)
     (service file-database-service-type
              (file-database-configuration
               (schedule #~'(next-hour '(6)))
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

     (service startx-command-service-type
              (xorg-configuration (modules (list xf86-input-libinput))))

     (simple-service 'resolvconf etc-service-type
                     (list `("resolvconf.conf"
                             ,(plain-file "resolvconf.conf"
                                          "name_servers=127.0.1.1\ndnsmasq=true\ndnsmasq_conf=/etc/dnsmasq.servers"))))

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
     (udev-rules-service 'qmk qmk-udev-rules)
     (udev-rules-service 'brightness brightnessctl)

     (simple-service
      'nongux-substitutes guix-service-type
      (guix-extension
       (substitute-urls '("https://substitutes.nonguix.org"))
       (authorized-keys (list %non-guix.pub))))))))
