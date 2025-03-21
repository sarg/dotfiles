(define-module (personal packages kindle)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
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
                (sh (search-input-file %build-inputs "/bin/sh"))
                (ip (search-input-file %build-inputs "/sbin/ip"))
                (bin (string-append out "/bin"))
                (kindle-start (string-append bin "/kindle_start"))
                (rules (string-append out "/lib/udev/rules.d"))
                (rule "SUBSYSTEM==\"net\", ACTION==\"~a\", ENV{ID_VENDOR}==\"Linux_3.0.35-lab126_with_fsl-usb2-udc\", NAME=\"eth_kindle\" RUN+=\"~a\""))
           (mkdir-p rules)
           (mkdir-p bin)
           (call-with-output-file kindle-start
             (lambda (port)
               (format port "#!~a
~a link set eth_kindle up
~a addr add 192.168.15.243/24 dev eth_kindle" sh ip ip)))
           (chmod kindle-start #o740)
           (call-with-output-file (string-append rules "/98-kindle-network.rules")
             (lambda (port)
               (format port rule "add" kindle-start)))))))
    (inputs
     (list bash-minimal iproute))
    (synopsis "udev rules for UBSNet kindle hack")
    (description "udev rules for UBSNet kindle hack")
    (license license:unlicense)
    (home-page "https://wiki.mobileread.com/wiki/USBNetwork")))
