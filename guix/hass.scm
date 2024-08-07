(use-modules (gnu)
             (gnu services)
             (gnu packages)
             (guix records)
             (guix packages)
             (guix channels)
             (guix gexp)
             (guix utils)
             (srfi srfi-1)
             (ice-9 textual-ports))

(use-package-modules linux ssh admin guile-xyz docker)

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

(define bridge-shepherd-service-type
  (shepherd-service-type
   'bridge-interface
   (lambda (config)
     (shepherd-service
      (documentation "Add interface to bridge")
      (provision (list (string->symbol (string-append "net-" (car config)))))
      (requirement '(udev))
      (modules '((ip link)))
      (start (with-extensions (list guile-netlink)
               #~(lambda _
                   (wait-for-link #$(cdr config) #:blocking? #f)
                   (link-add #$(car config) "bridge")
                   (link-set #$(car config) #:up #t)
                   (link-set #$(cdr config) #:up #t #:master #$(car config)))))
      (stop (with-extensions (list guile-netlink)
              #~(lambda _
                  (let ((ip (string-append #$iproute "/sbin/ip")))
                    (system* ip "link" "set" #$(cdr config) "nomaster")
                    ;; (link-set #$(cdr config) #:nomaster #t)
                    (link-set #$(cdr config) #:down #t)
                    (link-del #$(car config))))))))
   (description "Add interfaces to a bridge.")))

(define fs-2tb-disk
  (file-system
    (mount-point "/media/2tb")
    (type "xfs")
    (device (uuid "c3be1933-fc90-468e-8489-652f4f6b31ba"))
    (flags '(no-exec shared no-suid))
    (create-mount-point? #t)))

(define %user "sarg")
(define %user-uid 1000)

(define %shepherd-repl
  (shepherd-service-type
   'shepherd-repl
   (const (shepherd-service
           (documentation "Enable shepherd's repl.")
           (provision '(repl))
           (modules `((shepherd service repl) ,@%default-modules))
           (start #~((@@ (shepherd service) service-start) (repl-service)))
           (stop #~((@@ (shepherd service) service-stop) (repl-service)))))
   #f
   (description "Shepherd's REPL service.")))

(operating-system
  (timezone "Europe/Berlin")
  (host-name "hass")
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot"))))

  (sudoers-file
   (plain-file "sudoers"
               (string-append (plain-file-content %sudoers-specification)
                              (format #f "~a ALL = NOPASSWD: ALL~%"
                                      %user))))

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
                 (name %user)
                 (uid %user-uid)
                 (group "users")
                 (home-directory (string-append "/home/" %user))
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "tty" "input" "kvm" "dialout" "libvirt" "docker")))
                %base-user-accounts))

  (packages
   (append
    %base-packages

    (map specification->package
         '("tlp" "rsync" "borgmatic" "intel-vaapi-driver"
           "netcat"                     ; for spice viewer
           ))))

  (services
   (append
    (modify-services %base-services
      ;; key will be sent over by guix deploy
      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          (authorize-key? #f))))

    (list
     (service %shepherd-repl)
     (service libvirt-service-type)
     (service virtlog-service-type)
     (service ntp-service-type)
     (service elogind-service-type)

     (simple-service 'sysctl-custom
                     sysctl-service-type
                     '(("fs.inotify.max_user_watches" . "524288")))

     (service (shepherd-service-type
               'disk-2tb
               (@@ (gnu services base) file-system-shepherd-service)
               fs-2tb-disk
               (description "Mount disk.")))

     (service oci-container-service-type
              (list
               (let ((2tb (file-system-mount-point fs-2tb-disk)))
                 (oci-container-configuration
                  (image "jellyfin/jellyfin:10.8.12")
                  (requirement '(file-system-/media/2tb networking))
                  (container-user (format #f "~d:998" %user-uid))
                  (network "host")
                  (extra-arguments
                   (list
                    (string-append "--mount type=bind,source=" 2tb ",target=/media")
                    (string-append "--device /dev/dri")))

                  (volumes
                   (list
                    (format #f "~a/jellyfin/config:/config" 2tb)
                    (format #f "~a/jellyfin/cache:/cache" 2tb)))))))

     (service bridge-shepherd-service-type '("br0" . "enp0s25"))

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
               (wol-disable? #f)
               (restore-device-state-on-startup? #t)))

     (service openssh-service-type)))))
