(define-module (personal packages next)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu services xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages xorg))

;; supports tearfree in modesetting driver
(define-public xorg-server-next
  (let ((commit "c34f59ee152def40343c68fbdc3ee8f71a0d9575")
        (revision "1"))
    (package
     (inherit xorg-server-xwayland)
     (name "xorg-server-next")
     (version (git-version "22" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/xorg/xserver.git")
             (commit commit)))
       (sha256
        (base32
         "0vmncvpmxmdcs4kd3dd300k6367bkpaaip9lwvwza3jnwcrcnn6w"))))
     (inputs
      (append
       (package-inputs xorg-server-xwayland)
       (package-inputs xorg-server)))
     (propagated-inputs
      (package-propagated-inputs xorg-server)))))

(define-public xf86-input-libinput-next
  (package
    (inherit xf86-input-libinput)
    (inputs
     (modify-inputs (package-inputs xf86-input-libinput)
       (replace "xorg-server" xorg-server-next)))))

(define-public emacs-calibredb-next
  (let ((commit "40a16be1391e526c0a96e6eda3ba6bd44778e5d8")
        (revision "0"))
    (package
      (inherit emacs-calibredb)
      (name "emacs-calibredb")
      (version (git-version "2.13.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chenyanming/calibredb.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1a0a119fr4xznscjsgbjfmgn4xsp7kj4ylr5xl4s0xlqjcxjk4yr")))))))
