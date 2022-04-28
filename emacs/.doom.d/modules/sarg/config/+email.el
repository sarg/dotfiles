;;; private/sarg/email.el -*- lexical-binding: t; -*-

(defun sarg/ensure-msmtp-pass-available ()
  (interactive)
  (+pass/read-entry (concat "Email/" user-mail-address)))

(after! mu4e
  (setq mu4e-filter-inbox "flag:unread AND NOT flag:trashed AND maildir:/gmail/Inbox"
        mu4e-bookmarks '((mu4e-filter-inbox "Gmail messages" ?u)
                         ("date:today..now AND NOT flag:trashed AND NOT maildir:/gmail/trash" "Today's messages" 116))

        mu4e-alert-interesting-mail-query mu4e-filter-inbox)

  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . 8bit))
  (advice-add 'sendmail-send-it
              :before #'sarg/ensure-msmtp-pass-available)

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(set-email-account! "sarg"
  '((mu4e-sent-folder       . "/gmail/sent")
    (mu4e-drafts-folder     . "/gmail/drafts")
    (mu4e-trash-folder      . "/gmail/trash")
    (mu4e-refile-folder     . "/gmail/all")
    (smtpmail-smtp-user     . "sarg@sarg.org.ru")
    (user-mail-address      . "sarg@sarg.org.ru"))
  t)
