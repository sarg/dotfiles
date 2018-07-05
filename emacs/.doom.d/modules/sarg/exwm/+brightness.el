(defun sarg/brightness-change (amount)
  "Adjust screen brightness relatively using the amount given"
  (interactive "NChange screen brightness by: ")
  (let ((brightness-string))
    (if (> 0 amount)
        (setq brightness-string (concat (number-to-string amount) "%"))
      (setq brightness-string (concat "+" (number-to-string amount) "%")))
    (start-process "brightness" nil "xbacklight" brightness-string)))

(defvar sarg-redshift-timer 'nil
  "Stores redshift-adjust timer")

(defun sarg/redshift-adjust ()
  (interactive)
  (start-process-shell-command "redshift" nil "redshift" "-m randr -o"))

(defun sarg/redshift-start ()
  (interactive)
  (unless sarg-redshift-timer
    (setq sarg-redshift-timer (run-at-time nil 60 #'sarg/redshift-adjust))))

(defun sarg/redshift-cancel ()
  (interactive)
  (when sarg-redshift-timer (cancel-timer sarg-redshift-timer))
  (start-process-shell-command "redshift" nil "redshift" "-x"))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                    `(lambda () (interactive) (sarg/brightness-change 10)))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                    `(lambda () (interactive) (sarg/brightness-change -10)))

(sarg/redshift-start)
