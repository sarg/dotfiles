;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! dmenu)
(package! pulseaudio-control :recipe (pulseaudio-control :repo "flexibeast/pulseaudio-control" :fetcher github :branch "support-default-sink"))
(package! xelb :recipe (:fetcher github :repo "ch11ng/xelb" :commit "fe1b643e98ea4a87a3eed41b0bbaf6c12dfcfbec"))
(package! exwm :recipe (:fetcher github :repo "ch11ng/exwm" :commit "b75c89cae2a1c4c70044f885c44a95fd2f9950dd"))
