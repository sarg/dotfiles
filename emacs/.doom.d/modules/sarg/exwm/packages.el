;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! dmenu)
(package! circadian)
;; (package! gpastel)
(package! xelb
  :recipe (:host github :repo "ch11ng/xelb"))
(package! exwm
  :recipe (:host github :repo "ch11ng/exwm"))
(package! color-theme-sanityinc-tomorrow)
(package! anti-zenburn-theme)
(package! kaolin-themes)
(package! nord-theme)
(package! frame-purpose)
(package! frame-workflow :recipe (:host github :repo "akirak/frame-workflow"))
(package! backlight)
(package! statusbar
  :recipe (:host github :repo "dakra/statusbar.el"))
;; (package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff"))
