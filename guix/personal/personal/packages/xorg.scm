(define-module (personal packages xorg)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module  (gnu services xorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages xorg))

(define-public xorg-server-next
  (package
    (inherit xorg-server-xwayland)
    (name "xorg-server-next")
    (version "22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/xorg/xserver.git")
             (commit "37b7ea8f8aaef3efa9d56fb9bc82adeccba02633")))
       (sha256
        (base32
         "0sfji9lhp55wfibykv2sqckr3ax2kxssd4qd196wm7pqxbrxaf5k"))))
    (inputs
     (append
      (package-inputs xorg-server-xwayland)
      (package-inputs xorg-server)))
    (propagated-inputs
     (package-propagated-inputs xorg-server))))

(define-public xf86-input-libinput-next
  (package
    (inherit xf86-input-libinput)
    (inputs
     (modify-inputs (package-inputs xf86-input-libinput)
       (replace "xorg-server" xorg-server-next)))))

(define-public startx
  (xorg-start-command-xinit
   (xorg-configuration
    (server xorg-server-next)
    (modules (list xf86-input-libinput-next)))))
