;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! dmenu)
(package! bluetooth)
;; (package! gpastel)
(package! pulseaudio-control)
(package! frame-purpose)
(package! app-launcher :recipe (:host github :repo "sarg/app-launcher"))
;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))

(package! backlight)
(package! xelb :built-in 'prefer)
(package! exwm :built-in 'prefer)
(package! exwm-ss :recipe (:host codeberg :repo "emacs-weirdware/exwm-ss"))
(package! exwm-edit)

;; udisks
(package! debase :recipe (:host codeberg :repo "emacs-weirdware/debase"))
(package! discomfort :recipe (:host codeberg :repo "emacs-weirdware/discomfort"))
