(defvar sarg-redshift-timer 'nil
  "Stores redshift-adjust timer")

(defun sarg/redshift-adjust ()
  (interactive)
  (start-process-shell-command "redshift" nil "redshift -m randr -Po"))

(defun sarg/redshift-start ()
  (interactive)
  (unless sarg-redshift-timer
    (setq sarg-redshift-timer (run-at-time nil 60 #'sarg/redshift-adjust))))

(defun sarg/redshift-cancel ()
  (interactive)
  (when sarg-redshift-timer (cancel-timer sarg-redshift-timer))
  (start-process-shell-command "redshift" nil "redshift -x"))

(sarg/redshift-start)
