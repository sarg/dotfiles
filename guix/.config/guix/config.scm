(use-modules (gnu)
             (gnu services)
             (guix packages)
             (srfi srfi-1)
             (nongnu packages linux)
             (nongnu system linux-initrd))

(use-package-modules
 linux ssh android suckless
 xdisorg pulseaudio connman xorg)

(use-service-modules
 desktop ssh networking sysctl
 xorg dbus shepherd sound pm)


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

(operating-system
  (kernel linux-5.4)
  (kernel-arguments '("quiet" "loglevel=1"))
  (initrd microcode-initrd)
  (initrd-modules (cons "i915" %base-initrd-modules))
  (firmware (cons* iwlwifi-firmware %base-firmware))
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
    ;; xorg-server
    ;; xf86-input-libinput xf86-video-intel
    brightnessctl
    tlp
    %base-packages))

  (services (cons*
             (service connman-service-type)
             (service wpa-supplicant-service-type)

             (screen-locker-service slock)
             ;; (screen-locker-service xlockmore "xlock")

             ;; Add polkit rules, so that non-root users in the wheel group can
             ;; perform administrative tasks (similar to "sudo").
             polkit-wheel-service

             (udisks-service)
             (service polkit-service-type)
             (elogind-service)

             (service sysctl-service-type
                      (sysctl-configuration
                       (settings '(("kernel.printk" . "2 4 1 7")))))

             ;; fhs-binaries-compatibility-service
             (dbus-service)
             x11-socket-directory-service
             (service tlp-service-type)
             (service pulseaudio-service-type)
             (service alsa-service-type)

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
                         (udev-configuration-rules config)))))))))
