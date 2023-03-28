;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! frame-purpose :pin "57557dd87f2c43a1e43439f851e8d315da0052e2")
(package! app-launcher :recipe (:host github :repo "sarg/app-launcher") :pin "713294ae5b6c86ea9bcb5fa248bc860615fbf78b")
;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))

(package! xelb :built-in 'prefer)
(package! exwm :built-in 'prefer)
(package! exwm-ss :recipe (:host codeberg :repo "emacs-weirdware/exwm-ss") :pin "b11d3df7a50c39b4e1b92ef8a6685cf80b53912c")
(package! exwm-edit :pin "b5b7e950f57e30befd68d51df34540b70e6ac28f")
