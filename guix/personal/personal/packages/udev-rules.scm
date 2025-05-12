(define-module (personal packages udev-rules)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash))

(define-public kindle-usbnet-udev-rules
  (package
    (name "kindle-usbnet-udev-rules")
    (version "1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (ip (search-input-file %build-inputs "/sbin/ip"))
                (rules (string-append out "/lib/udev/rules.d")))
           (mkdir-p rules)
           (call-with-output-file (string-append rules "/98-kindle-network.rules")
             (lambda (port)
               (format port
                       (string-join
                        '("SUBSYSTEM==\"net\""
                          "ACTION==\"add\""
                          "ENV{ID_VENDOR}==\"Linux_3.0.35-lab126_with_fsl-usb2-udc\""
                          "RUN+=\"~a link set %k up\""
                          "RUN+=\"~a add add 192.168.15.243/24 dev %k\"")
                        ", ")
                       ip ip)))))))
    (inputs (list iproute))
    (synopsis "udev rules for UBSNet kindle hack")
    (description "udev rules for UBSNet kindle hack")
    (license license:unlicense)
    (home-page "https://wiki.mobileread.com/wiki/USBNetwork")))

(define-public pixel-usbnet-udev-rules
  (package
    (name "pixel-usbnet-udev-rules")
    (version "1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (rules (string-append out "/lib/udev/rules.d")))
           (mkdir-p rules)
           (call-with-output-file
               (string-append rules "/98-pixel-network.rules")
             (lambda (port)
               (format port
                       (string-join
                        '("SUBSYSTEM==\"net\""
                          "ACTION==\"add\""
                          "ENV{ID_USB_DRIVER}==\"cdc_ncm\""
                          "RUN+=\"~a %k\"")
                        ", ")
                       (search-input-file %build-inputs "/sbin/dhcpcd"))))))))
    (inputs (list dhcpcd))
    (synopsis "udev rules for UBSNet pixel")
    (description "udev rules for UBSNet pixel")
    (license license:unlicense)
    (home-page #f)))

(define-public iwlwifi-led-udev-rules
  (package
   (name "iwlwifi-led-udev-rules")
   (version "1")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (rules (string-append out "/lib/udev/rules.d")))
          (mkdir-p rules)
          (call-with-output-file
              (string-append rules "/98-iwlwifi-led.rules")
            (lambda (port)
              (format port
                      "ACTION==~s, SUBSYSTEM==~s, KERNEL==~s, ATTR{trigger}=~s"
                      "add" "leds" "phy0-led" "phy0radio")))))))
   (synopsis "udev rules for iwlwifi led")
   (description "udev rules for iwlwifi led")
   (license license:unlicense)
   (home-page #f)))

(define-public backlight-udev-rules
  (package
   (name "backlight-udev-rules")
   (version "1")
   (source #f)
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (rules (string-append out "/lib/udev/rules.d")))
          (mkdir-p rules)
          (call-with-output-file
              (string-append rules "/98-backlight.rules")
            (lambda (port)
              (format port
                      "ACTION==~s, SUBSYSTEM==~s, ATTR{brightness}=~s"
                      "add" "backlight" "$attr{max_brightness}")))))))
   (synopsis "udev rules to set backlight brightness to max")
   (description "udev rules to set backlight brightness to max")
   (license license:unlicense)
   (home-page #f)))
