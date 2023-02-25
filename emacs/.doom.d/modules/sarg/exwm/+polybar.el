(defvar sarg/polybar-process nil)

(define-minor-mode sarg/polybar-mode
  "Show polybar."
  :init-value nil
  :global t

  (if sarg/polybar-mode
      (progn
        (exwm-workspace--update-ewmh-desktop-names)
        (setq sarg/polybar-process
              (start-process "polybar" nil
                             "polybar" "panel")))

    (when sarg/polybar-process
      (interrupt-process sarg/polybar-process)
      (setq sarg/polybar-process nil))))

(add-hook 'exwm-init-hook #'sarg/polybar-mode)
