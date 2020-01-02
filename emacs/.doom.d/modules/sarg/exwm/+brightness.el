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

(defhydra hydra-brightness ()
    "brightness"
    ("<f8>" (progn (backlight-dec 100) (message "Brightness: %d" backlight--current-brightness)) "dec")
    ("<f9>" (progn (backlight-inc 100) (message "Brightness: %d" backlight--current-brightness)) "inc"))


(spacemacs/exwm-bind-command
 "<XF86MonBrightnessUp>"   `hydra-brightness/body
 "<XF86MonBrightnessDown>" `hydra-brightness/body)

(sarg/redshift-start)

;; Change color theme based on day time
(use-package! color-theme-sanityinc-tomorrow)
(use-package! kaolin-themes)
(use-package! anti-zenburn-theme)
(use-package! circadian
  :config
  (setq calendar-latitude 52.516667
        calendar-longitude 13.388889
        circadian-themes
        '(
          (:sunrise . kaolin-breeze)
          (:sunset . kaolin-valley-dark)
          ;; (:sunrise . sanityinc-tomorrow-day)
          ;; (:sunset . anti-zenburn)
          ))
  (circadian-setup))
