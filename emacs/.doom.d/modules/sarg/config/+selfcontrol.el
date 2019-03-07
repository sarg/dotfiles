(defun sarg/ad-selfcontrol-time-restriction (orig-fun &rest args)
  (if (or (= 0 (mod
                (calendar-day-of-week (calendar-current-date))
                6)) ; 0 and 6 - Sunday and Saturday
          (> (nth 2 (decode-time)) 18))
      (apply orig-fun args)
    (message "It's not the time yet!")))

(defun sarg/ad-selfcontrol-otp-confirm (proc &rest args)
  "Asks to enter random string as confirmation before executing PROC."
  (interactive)
  (let* ((pass (format "%06x%06x%06x"
                       (random (expt 16 6))
                       (random (expt 16 6))
                       (random (expt 16 6))))

         (input (read-string (format "Enter %s if you wish to proceed: " pass))))

    (if (string= input pass)
        (apply proc args))))

(after! elfeed
  (advice-add 'elfeed-update :around #'sarg/ad-selfcontrol-otp-confirm)
  (advice-add '=rss :around #'sarg/ad-selfcontrol-otp-confirm))
  ;;(advice-add 'elfeed-update :around #'sarg/selfcontrol-time-restriction))

;; (after! telega
;;   (advice-add 'ivy-telega-chat-with :around #'sarg/ad-selfcontrol-otp-confirm))


;; (defun sarg/test (time)
;;   (closure (proc &rest args)
;;     (message "%s" time)
;;     (apply proc args)))

;; (defun sarg/hello ()
;;   (message "Hiii!"))

;; (advice-add 'sarg/hello :around (sarg/test "test"))

;; (sarg/hello)
