(defvar exwm/lemonbar-mode-line-format '(""))
(defun exwm/lemonbar-update ()
  (process-send-string
   exwm/bar-process
   (concat "%{r}" (format-mode-line exwm/lemonbar-mode-line-format 0) " %{r}\n")))

(defvar exwm/lemonbar-process nil)
(defvar exwm/lemonbar-timer nil)

(defun exwm/lemonbar-restart (&rest args)
  (interactive)
  (when exwm/lemonbar-process
    (interrupt-process exwm/lemonbar-process))

  (let ((bw 200) (bh 18) (screen-w 1600))
    (setq exwm/lemonbar-process
          (start-process-shell-command
           "lemonbar" nil
           (concat
            "lemonbar -d"               ; force dock
            ;; geometry
            " -b -g " (format "%dx%d+%d+0" bw bh (- screen-w bw))
            " -f '" (face-font 'default) "'"
            " -o -2 "                   ; vertical offset for better visual
            ;; font color
            " -F '" (face-foreground 'default) "'")))))

;;;###autoload
(define-minor-mode exwm/lemonbar-mode
  "Show bar."
  :init-value nil
  :global t

  (cond
   (exwm/lemonbar-mode
    (exwm/lemonbar-restart)

    (advice-add 'enable-theme :after #'exwm/lemonbar-restart)
    (setq exwm/lemonbar-timer (run-with-timer 1 1 #'exwm/lemonbar-update)))

   (t
    (cancel-timer exwm/lemonbar-timer)
    (advice-remove 'enable-theme #'exwm/lemonbar-restart)
    (interrupt-process exwm/lemonbar-process)
    (setq exwm/lemonbar-process nil))))
