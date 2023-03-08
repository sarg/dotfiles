(after! ivy-rich
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)

  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-rich-candidate
                  (:width (lambda (x)
                            (if (eq 'exwm-mode (ivy-rich--local-values x 'major-mode))
                                x
                              (ivy-rich-normalize-width x 30)))))
                 (ivy-rich-switch-buffer-path
                  (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))))

  ;; reload to make new display-transformer work
  (ivy-rich-reload))
