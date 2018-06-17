(use-package telega
  :load-path  "~/devel/ext/telega.el"
  :commands (telega)
  :config ((set-popup-rule! "^\\\*Telega Root\*"
           '((size . 0.25) (side . left))
           '((quit . current) (select . t))

           (setq telega-completing-read-function 'ivy-completing-read)))
  :defer t)

(use-package telega-notifications
  :load-path  "~/devel/ext/telega.el"
  :after (telega)
  :config (add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1))))
