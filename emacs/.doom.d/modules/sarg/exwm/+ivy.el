
;; props to https://github.com/ch11ng/exwm/issues/593
(defun my-exwm-workspace-switch-to-buffer (orig-func buffer-or-name &rest args)
  (if-let* ((buf (get-buffer (or buffer-or-name (other-buffer))))
            (floating-p? (or exwm--floating-frame
                             (with-current-buffer buf exwm--floating-frame))))
      (exwm-workspace-switch-to-buffer buffer-or-name)
    (apply orig-func buffer-or-name args)))

(defun my/exwm-counsel-yank-pop ()
  "Same as `counsel-yank-pop' and paste into exwm buffer."
  (interactive)
  (let ((inhibit-read-only t)
        ;; Make sure we send selected yank-pop candidate to
        ;; clipboard:
        (yank-pop-change-selection t))
    (call-interactively #'counsel-yank-pop))
  (when (derived-mode-p 'exwm-mode)
    ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
    (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
    (exwm-input--fake-key ?\C-v)))

(advice-add 'ivy--switch-buffer-action :around 'my-exwm-workspace-switch-to-buffer)
(exwm-input-set-key (kbd "M-y") #'my/exwm-counsel-yank-pop)
(when (featurep! :app telega)
  (exwm-input-set-key (kbd "s-i") #'sarg/sauron-show))
