(defvar exwm/polybar-process nil)

(define-minor-mode exwm/polybar-mode
  "Show polybar."
  :init-value nil
  :global t

  (if exwm/polybar-mode
      (progn
        (exwm-workspace--update-ewmh-desktop-names)
        (setq exwm/polybar-process
              (start-process "polybar" nil
                             "polybar" "panel")))

    (when exwm/polybar-process
      (interrupt-process exwm/polybar-process)
      (setq exwm/polybar-process nil))))

(add-hook 'exwm-init-hook #'exwm/polybar-mode)
