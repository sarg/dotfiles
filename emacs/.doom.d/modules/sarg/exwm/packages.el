;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! dmenu)
(package! circadian)
(package! bluetooth)
;; (package! gpastel)
(package! xelb
  :recipe (:host github :repo "ch11ng/xelb"))
(package! exwm
  :recipe (:host github :repo "ch11ng/exwm"))
(package! color-theme-sanityinc-tomorrow)
(package! anti-zenburn-theme)
(package! kaolin-themes)
(package! bufler)
(package! nord-theme)
(package! pulseaudio-control)
(package! frame-purpose)
(package! frame-workflow :recipe (:host github :repo "akirak/frame-workflow"))
(package! exwm-ss :recipe (:host codeberg :repo "emacs-weirdware/exwm-ss"))
(package! backlight)
(package! exwm-edit)
(package! app-launcher :recipe (:host github :repo "sarg/app-launcher"))
;; :recipe (:host github :repo "sarg/exwm-edit" :branch "fix-display")

;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))
