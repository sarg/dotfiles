;; -*- no-byte-compile: t; -*-
;;; app/exwm/packages.el

(package! app-launcher :recipe (:host github :repo "sarg/app-launcher") :pin "31289d1150d8e26190d0600a68cd21ee025e5418")
;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))

(package! xelb :built-in 'prefer)
(package! exwm :built-in 'prefer)
(package! exwm-ss :built-in 'prefer
  :recipe (:host codeberg :repo "emacs-weirdware/exwm-ss") :pin "b11d3df7a50c39b4e1b92ef8a6685cf80b53912c")
(package! exwm-edit :pin "046b8c11f71bfd6c798df770c6b7708af2c187a2")
