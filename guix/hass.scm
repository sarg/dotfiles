(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix packages)
             (guix channels)
             (guix utils)
             (srfi srfi-1)
             (ice-9 textual-ports))

(use-package-modules linux ssh admin guile-xyz)

(use-service-modules
 desktop ssh networking sysctl docker
 dbus shepherd pm dns virtualization)

(define polkit-udisks-wheel
  (file-union
   "polkit-udisks-wheel"
   `(("share/polkit-1/rules.d/udisks-wheel.rules"
      ,(local-file "./files/udisks-wheel.rules")))))

(define polkit-udisks-wheel-service
  (simple-service 'polkit-udisks-wheel polkit-service-type (list polkit-udisks-wheel)))

(define (bridge-shepherd-service config)
  (list (shepherd-service
         (documentation "Add interface to bridge")
         (provision (list (string->symbol (string-append "net-" (car config)))))
         (requirement '(udev))
         (modules '((ip link)))
         (start (with-extensions (list guile-netlink)
                  #~(lambda _
                      (wait-for-link #$(cdr config) #:blocking? #f)
                      ;; (link-set #$(car config) #:up #t #:master #(cdr config))
                      (let ((ip (string-append #$iproute "/sbin/ip")))
                        (system* ip "link" "add" "name" #$(car config) "type" "bridge")
                        (system* ip "link" "set" #$(car config) "up")
                        (system* ip "link" "set" #$(cdr config) "up")
                        (system* ip "link" "set" #$(cdr config) "master" #$(car config))))))
         (stop (with-extensions (list guile-netlink)
                 #~(lambda _
                     ;; (link-set #$(car config) #:down #t #:nomaster #t)
                     (let ((ip (string-append #$iproute "/sbin/ip")))
                       (system* ip "link" "set" #$(cdr config) "nomaster")
                       (system* ip "link" "set" #$(cdr config) "down")
                       (system* ip "link" "delete" #$(car config)))))))))

(operating-system
  (timezone "Europe/Berlin")
  (host-name "hass")
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
                  '("wheel" "netdev" "audio" "video" "tty" "input" "kvm" "dialout" "libvirt" "docker")))
                %base-user-accounts))

  (packages
   (append
    %base-packages

    (map specification->package
         '("nss-certs" "tlp" "rsync" "borgmatic"
           "intel-vaapi-driver"))))

  (services
   (append
    (modify-services %base-services
      (sysctl-service-type config =>
                           (sysctl-configuration
                            (inherit config)
                            (settings (append
                                       (sysctl-configuration-settings config)
                                       '(("fs.inotify.max_user_watches" . "524288")
                                         ("net.ipv6.conf.all.disable_ipv6" . "1")))))))

    (list
     (service libvirt-service-type)
     (service virtlog-service-type)
     (service docker-service-type)
     (service ntp-service-type)
     (service elogind-service-type)

     (simple-service 'br0-enp0s25
                    shepherd-root-service-type
                    (bridge-shepherd-service
                     '("br0" . "enp0s25")))

     (service dhcp-client-service-type
              (dhcp-client-configuration
               (shepherd-requirement '(net-br0))
               (interfaces '("br0"))))

     (simple-service 'dhclient-wan etc-service-type
                     (list `("dhclient.conf"
                             ,(plain-file "dhclient.conf" "send host-name = gethostname();"))))

     ;; Add polkit rules, so that non-root users in the wheel group can
     ;; perform administrative tasks (similar to "sudo").
     polkit-wheel-service
     polkit-udisks-wheel-service

     (service udisks-service-type)
     (service polkit-service-type)
     (service dbus-root-service-type)

     (service tlp-service-type
              (tlp-configuration
               (restore-device-state-on-startup? #t)))

     (service openssh-service-type
              (openssh-configuration
               (x11-forwarding? #t)))))))
