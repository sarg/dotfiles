;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! dmenu :pin "e8cc9b27c79d3ecc252267c082ab8e9c82eab264")
(package! bluetooth :pin "fc6250c25c0f8d77cd5e637f97c5ed8d7937041b")
;; (package! gpastel)
(package! pulseaudio-control :pin "e917e84661b0e2496b295f1bbfba6ad32a656527")
(package! frame-purpose :pin "57557dd87f2c43a1e43439f851e8d315da0052e2")
(package! app-launcher :recipe (:host github :repo "sarg/app-launcher") :pin "713294ae5b6c86ea9bcb5fa248bc860615fbf78b")
;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))

(package! backlight :pin "b6826a60440d8bf440618e3cdafb40158de920e6")
(package! xelb :built-in 'prefer)
(package! exwm :built-in 'prefer)
(package! exwm-ss :recipe (:host codeberg :repo "emacs-weirdware/exwm-ss") :pin "b11d3df7a50c39b4e1b92ef8a6685cf80b53912c")
(package! exwm-edit :pin "b5b7e950f57e30befd68d51df34540b70e6ac28f")

;; udisks
(package! debase :recipe (:host codeberg :repo "emacs-weirdware/debase") :pin "0b6fc2af3440d68798e3a85d4c889341aae07936")
(package! discomfort :recipe (:host codeberg :repo "emacs-weirdware/discomfort") :pin "873eea833bbae7196b92bb1102494b8bf5dc5df6")
