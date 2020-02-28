;; (exwm-systemtray--exit)

(defun dw/polybar-exwm-workspace ()
  (+ 1 exwm-workspace-current-index))

(defun dw/update-polybar-exwm ()
  (f-append-text (format "%s\n" (dw/polybar-exwm-workspace))
                 'utf-8
                 "/tmp/exwm.workspace"))

(add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
