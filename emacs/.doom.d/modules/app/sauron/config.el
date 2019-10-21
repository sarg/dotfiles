;;; app/sauron/config.el -*- lexical-binding: t; -*-

(defun sarg/sauron-show ()
  (interactive)
  (sauron-start)
  (pop-to-buffer (sr-create-buffer-maybe sr-buffer-name)))

(use-package! sauron
  :config

  (setq sauron-modules '(sauron-dbus sauron-org))
  (setq sauron-separate-frame nil)

  (set-popup-rule! (regexp-quote sr-buffer-name)
    :size 0.25 :side 'bottom
    :select t :quit t :ttl nil)

  (when (featurep! :app telega)
    (load! "sauron-telega")
    (add-to-list 'sauron-modules 'sauron-telega)
    (sauron-telega-start))

  (after! slack
    (load! "sauron-slack")
    (add-to-list 'sauron-modules 'sauron-slack)
    (sauron-slack-start))

  (when (featurep! :editor evil)
    (add-to-list 'evil-normal-state-modes 'sauron-mode)

    (map! :map sauron-mode-map
          :n "RET" #'sauron-activate-event
          :n "c" #'sauron-clear))

  (sauron-start-hidden))
