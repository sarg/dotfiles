;;; app/sauron/sauron-slack.el -*- lexical-binding: t; -*-

(defvar sauron-prio-slack 3
  "slack event priority.")

(defun sauron-slack-start ()
  "Starts sauron-slack."
  (setq slack-message-custom-notifier #'sauron-slack-hook))

(defun sauron-slack-stop ()
  (setq slack-message-custom-notifier nil))

(defun sauron-slack-hook (message room team)
  "Hook which is called on new messages."

  (when (slack-message-notify-p message room team)
    (let ((team-name (oref team name))
          (room-name (slack-room-name room team))
          (text (with-temp-buffer
                  (goto-char (point-min))
                  (insert (slack-message-to-alert message team))
                  (slack-buffer-buttonize-link)
                  (buffer-substring-no-properties (point-min)
                                                  (point-max))))
          (user-name (slack-message-sender-name message team)))
      (sauron-add-event
       'slack sauron-prio-slack
       (if (slack-im-p room) text (format "%s: %s" user-name text))
       nil
       `(:sender ,user-name)
       ))))

(provide 'sauron-slack)
