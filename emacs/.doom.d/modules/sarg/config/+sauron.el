;;; sarg/config/+sauron.el -*- lexical-binding: t; -*-

(def-package! sauron
  :config

  (setq sauron-separate-frame nil)
  (sauron-dbus-start)

  (set-popup-rule! sr-buffer-name
    :size 0.25 :actions '(display-buffer-below-selected)
    :select t :quit t :ttl nil))
