;;; private/sarg/autoload/email.el -*- lexical-binding: t; -*-

;;;###autoload
(defun =email ()
  "Start email client."
  (interactive)
  (require 'mu4e)
  (call-interactively #'mu4e-goto-unread))

;;;###autoload
(defun +email/compose ()
  "Compose a new email."
  (interactive)
  ;; TODO Interactively select email account
  (call-interactively #'mu4e-compose-new))

