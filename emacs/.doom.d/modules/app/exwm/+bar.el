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

(setq battery-mode-line-format "%b%>2p%"
      battery-mode-line-limit 50)
(display-battery-mode 1)

(defun exwm/bar-update ()
  (let* ((in-exwm (buffer-local-value 'exwm--id (current-buffer)))
         (layout (if-let ((in-exwm)
                          (group (exwm-input--current-group)))
                     (when (> group 0) (nth group exwm/input-layout-names))
                   evil-input-method))
         (layout-str (and layout (downcase (substring layout 0 2))))
         (time-str (format-time-string "%a %d %R")))

    (process-send-string
     exwm/bar-process
     (concat "%{r}"
      (string-join (-remove #'s-blank?
                            (list battery-mode-line-string layout-str time-str))
       " ") " %{r}\n"))))

(defvar exwm/bar-process nil)
(defvar exwm/bar-timer nil)
(define-minor-mode exwm/bar-mode
  "Show bar."
  :init-value nil
  :global t

  (cond
   (exwm/bar-mode
    (let ((bw 200) (bh 18))
      (setq exwm/bar-process
            (start-process-shell-command
             "lemonbar" nil
             (concat
              "lemonbar -d"             ; force dock
              ;; geometry
              " -b -g " (format "%dx%d+%d+0" bw bh (- 1600 bw))
              " -f Hack:size=13"
              " -o -2 "                 ; vertical offset
              ;; font color
              " -F '" (face-attribute 'default :foreground) "'"))))
    (setq exwm/bar-timer (run-with-timer 1 1 #'exwm/bar-update)))

   (t
    (cancel-timer exwm/bar-timer)
    (interrupt-process exwm/bar-process)
    (setq exwm/bar-process nil))))
