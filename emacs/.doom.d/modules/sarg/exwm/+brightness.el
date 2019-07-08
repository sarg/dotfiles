(defvar sarg-redshift-timer 'nil
  "Stores redshift-adjust timer")

(defun sarg/redshift-adjust ()
  (interactive)
  (start-process-shell-command "redshift" nil "redshift" "-m randr -Po"))

(defun sarg/redshift-start ()
  (interactive)
  (unless sarg-redshift-timer
    (setq sarg-redshift-timer (run-at-time nil 60 #'sarg/redshift-adjust))))

(defun sarg/redshift-cancel ()
  (interactive)
  (when sarg-redshift-timer (cancel-timer sarg-redshift-timer))
  (start-process-shell-command "redshift" nil "redshift" "-x"))

(spacemacs/exwm-bind-command
 "<XF86MonBrightnessUp>"   `(lambda () (interactive) (backlight-inc 100))
 "<XF86MonBrightnessDown>" `(lambda () (interactive) (backlight-dec 100)))

(sarg/redshift-start)

;; Change color theme based on day time
(def-package! color-theme-sanityinc-tomorrow)
(def-package! kaolin-themes)
(def-package! anti-zenburn-theme)
(def-package! circadian
  :config
  (setq calendar-latitude 56
        calendar-longitude 38
        circadian-themes
        '(
          (:sunrise . kaolin-breeze)
          (:sunset . kaolin-valley-dark)
          ;; (:sunrise . sanityinc-tomorrow-day)
          ;; (:sunset . anti-zenburn)
          ))
  (circadian-setup))
