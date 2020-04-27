(use-package! bufler
  :config
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "C-k") 'bufler-list-buffer-kill
    "r" 'bufler
    (kbd "RET") 'bufler-list-buffer-switch)

  (setq bufler-groups
        (bufler-defgroups
          (group (mode-match "Telega" (rx bos "telega-chat")))
          (group
           (lambda (buffer)
             (and
              (funcall (mode-match "exwm-mode" (rx bos "exwm-mode")) buffer)
              "X11"))
           (group
            (lambda (buffer)
              (and
               (string= "qutebrowser"
                        (buffer-local-value 'exwm-class-name buffer))
               "WWW"))))
          (group (auto-project))
          (auto-directory))))
