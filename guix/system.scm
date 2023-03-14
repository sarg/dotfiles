(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix transformations)
             (guix packages)
             (guix channels)
             (guix inferior)
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

(define %grub-lubuntu-14 "
menuentry \"Lubuntu 14.04 ISO\" {
    set isofile=\"/home/sarg/focal-desktop-amd64.iso\"
    loopback loop ($root)$isofile
    linux (loop)/casper/vmlinuz boot=casper iso-scan/filename=${isofile} quiet splash
    initrd (loop)/casper/initrd
}
")


(define dhclient-enter-hooks "
make_resolv_conf() {
    touch /etc/dnsmasq.servers
    sed -i '/#dhcp/,+1d' /etc/dnsmasq.servers
    cat <<EOF >>/etc/dnsmasq.servers
#dhcp
server=/${new_domain_name}/${new_domain_name_servers}
EOF

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

(define-public (manifest->packages manifest) ;; copied from guix/scripts/refresh.scm, referring to it with (@@ ...) stopped working for some reason, kept saying it's an unbound variable
  "Return the list of packages in MANIFEST."
  (filter-map (lambda (entry)
                (let ((item (manifest-entry-item entry)))
                  (if (package? item) item #f)))
              (manifest-entries manifest)))

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

(define wifi-udev-rule
  (udev-rule
   "80-wifi.rules"
   "SUBSYSTEM==\"net\", ACTION==\"add\", DRIVERS==\"iwlwifi\", KERNEL==\"wl*\", NAME=\"wifi\""))

(operating-system
  (kernel linux)
  (kernel-arguments '("quiet" "loglevel=1" "ipv6.disable=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware broadcom-bt-firmware %base-firmware))
  (locale "en_GB.UTF-8")
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
   (cons*
    (specification->package "nss-certs")
    (specification->package "bluez")
    brightnessctl
    tlp
    libratbag
    (filter (lambda (p)
              (not (member (package-name p)
                           '("wireless-tools" "info-reader" "nano" "mg"))))
            %base-packages)))

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
             (extra-special-file "/etc/dhclient-enter-hooks"
                                 (plain-file "dhclient-enter-hooks"
                                             dhclient-enter-hooks))
             (extra-special-file "/etc/dhclient.conf"
                                 (plain-file "dhclient.conf"
                                             "send host-name = gethostname();"))

             (service screen-locker-service-type
                      (screen-locker-configuration
                       "physlock" (file-append physlock "/bin/physlock") #f))

             ;; Add polkit rules, so that non-root users in the wheel group can
             ;; perform administrative tasks (similar to "sudo").
             polkit-wheel-service
             (service bluetooth-service-type)

             (service udisks-service-type)
             (service polkit-service-type)
             (service elogind-service-type
                      (elogind-configuration
                       (handle-lid-switch-external-power 'suspend)))

             (service dbus-root-service-type)
             (simple-service 'ratbagd dbus-root-service-type `(,libratbag))
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

             (service sysctl-service-type
                      (sysctl-configuration
                       (settings (append
                                  %default-sysctl-settings
                                  '(("fs.inotify.max_user_watches" . "524288")
                                    ("net.ipv6.conf.all.disable_ipv6" . "1"))))))

             (modify-services %base-services
               (delete sysctl-service-type)
               (guix-service-type config =>
                                  (guix-configuration
                                   (inherit config)
                                   (substitute-urls
                                    (append (list "https://substitutes.nonguix.org")
                                            %default-substitute-urls))
                                   (authorized-keys
                                    (append (list (plain-file "non-guix.pub"
                                                              "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                            %default-authorized-guix-keys))))))))
