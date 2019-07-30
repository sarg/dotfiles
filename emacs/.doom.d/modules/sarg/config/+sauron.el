;;; sarg/config/+sauron.el -*- lexical-binding: t; -*-

(defun sarg/sauron-show ()
  (interactive)
  (sauron-start)
  (pop-to-buffer (sr-create-buffer-maybe sr-buffer-name)))

(def-package! sauron
  :config

  (setq sauron-modules '(sauron-dbus sauron-org))
  (setq sauron-separate-frame nil)

  (set-popup-rule! (regexp-quote sr-buffer-name)
    :size 0.25 :side 'bottom
    :select t :quit t :ttl nil)

  (when (featurep! :editor evil)
    (add-to-list 'evil-normal-state-modes 'sauron-mode)

    (map! :map sauron-mode-map
          :n "RET" #'sauron-activate-event
          :n "c" #'sauron-clear))

  (sauron-start-hidden))
