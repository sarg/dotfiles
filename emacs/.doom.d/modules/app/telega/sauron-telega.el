;;; app/telega/sauron-telega.el -*- lexical-binding: t; -*-

(defvar sauron-prio-telega 3
  "telega event priority.")

(defvar sauron-telega-running nil
  "when non-nil, sauron-telega is running")

(defun sauron-telega-start ()
  "Starts sauron-telega."
  (when sauron-telega-running
    (error "sauron-telega is already running. Call
          sauron-telega-stop first."))

  (add-hook 'telega-chat-post-message-hook #'sauron-telega-hook)
  (setq sauron-telega-running t))

(defun sauron-telega-stop ()
  "Stops and cleans up sauron-telega."
  (when sauron-telega-running
    (remove-hook 'telega-chat-post-message-hook #'sauron-telega-hook)
    (setq sauron-telega-running nil)))

(defun sauron-telega-hook (msg)
  "Hook which is called on new messages."

  (let* ((msg-id (plist-get msg :id))
         (chat (telega-msg-chat msg))
         (username (telega-chat-username chat)))

    (unless (or (telega-msg-ignored-p msg)
                (> (- (time-to-seconds) (plist-get msg :date)) 60)
                (not (zerop (telega-chat-notification-setting chat :mute_for)))
                (telega-msg-seen-p msg chat)
                (telega-msg-observable-p msg chat))

      (sauron-add-event
       'telega sauron-prio-telega
       (telega--desurrogate-apply
        (telega-ins--as-string
         (funcall telega-inserter-for-msg-notification msg)))
       (lambda () (telega-chat--goto-msg chat msg-id t))
       `(:sender ,username)))))

(provide 'sauron-telega)
