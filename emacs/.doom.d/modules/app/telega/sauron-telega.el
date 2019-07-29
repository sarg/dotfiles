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

  (add-hook 'telega-chat-pre-message-hook #'sauron-telega-hook)
  (setq sauron-telega-running t))

(defun sauron-telega-stop ()
  "Stops and cleans up sauron-telega."
  (when sauron-telega-running
    (remove-hook 'telega-chat-pre-message-hook #'sauron-telega-hook)
    (setq sauron-telega-running nil)))

(defun sauron-telega-hook (msg)
  "Hook which is called on new messages."

  (let* ((msg-id (plist-get msg :id))
         (chat-id (plist-get msg :chat_id))
         (chat (telega-chat-get chat-id))
         (user-id (plist-get msg :sender_user_id))
         (username (telega-chat-username chat)))

    (sauron-add-event
     'telega sauron-prio-telega
     (telega--desurrogate-apply
      (telega-ins--as-string
       (funcall telega-inserter-for-msg-notification msg)))
     (lambda () (telega-chat--goto-msg chat msg-id t))
     `(:sender ,username))))

(provide 'sauron-telega)
