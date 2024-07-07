(defun exwm/bar-input-layout-segment ()
  (if-let ((is-exwm-window? (buffer-local-value 'exwm--id (current-buffer)))
           (group (exwm-input--current-group)))
      (when (> group 0) (nth group exwm/input-layout-names))
    evil-input-method))

(defvar exwm/bar-mode-line-format '(""))
(defun exwm/bar-update ()
  (process-send-string
   exwm/bar-process
   (concat "%{r}" (format-mode-line exwm/bar-mode-line-format 0) " %{r}\n")))

(defvar exwm/bar-process nil)
(defvar exwm/bar-timer nil)

(defun exwm/bar-restart (&rest args)
  (interactive)
  (when exwm/bar-process
    (interrupt-process exwm/bar-process))

  (let ((bw 200) (bh 18) (screen-w 1600))
    (setq exwm/bar-process
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
(define-minor-mode exwm/bar-mode
  "Show bar."
  :init-value nil
  :global t

  (cond
   (exwm/bar-mode
    (exwm/bar-restart)

    (advice-add 'enable-theme :after #'exwm/bar-restart)
    (setq exwm/bar-timer (run-with-timer 1 1 #'exwm/bar-update)))

   (t
    (cancel-timer exwm/bar-timer)
    (advice-remove 'enable-theme #'exwm/bar-restart)
    (interrupt-process exwm/bar-process)
    (setq exwm/bar-process nil))))
