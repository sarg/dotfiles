;;; private/sarg/email.el -*- lexical-binding: t; -*-

;; I want to live in Emacs. Living is incomplete without email, so Emacs needs
;; to give me the ability to read, search, write and send my email. It does so
;; with `mu4e', and requires `offlineimap' and `mu' to be installed.

(defun mu4e-goto-unread ()
  (interactive)
  ;; (mu4e-update-mail-and-index nil)
  (mu4e-headers-search (mu4e-get-bookmark-query ?u)))


(defun mu4e-message-maildir-matches (msg rx)
  (when rx
    (if (listp rx)
        ;; if rx is a list, try each one for a match
        (or (mu4e-message-maildir-matches msg (car rx))
            (mu4e-message-maildir-matches msg (cdr rx)))
      ;; not a list, check rx
      (string-match rx (mu4e-message-field msg :maildir)))))

;; Choose account label to feed msmtp -a option based on From header
;; in Message buffer; This function must be added to
;; message-send-mail-hook for on-the-fly change of From address before
;; sending message since message-send-mail-hook is processed right
;; before sending message.
(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "sarg@sarg.org.ru" from) "gmail"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

;;
;; Config
;;

(after! mu4e
  (add-to-list 'mu4e-headers-actions '("browser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("browser" . mu4e-action-view-in-browser) t)

  (setq mu4e-user-mail-address-list '("sarg@sarg.org.ru"))

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "gmail"
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-maildir-matches msg "^/gmail")))
            :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "d") 'mu4e-move-to-trash))
            :vars '(
                    ;; local directories, relative to mail root
                    (mu4e-sent-folder . "/gmail/sent")
                    (mu4e-drafts-folder . "/gmail/drafts")
                    (mu4e-trash-folder . "/gmail/trash")
                    (mu4e-refile-folder . "/gmail/all")
                    ;; account details
                    (user-mail-address . "sarg@sarg.org.ru")
                    ;; gmail saves every outgoing message automatically
                    (mu4e-sent-messages-behavior . delete)
                    (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?j)
                                               ("/gmail/all" . ?a)
                                               ("/gmail/trash" . ?t)))
                    (mu4e-headers-skip-duplicates . t)))
          )))

;;
;; Plugins
;;
;;
(defun sarg/ensure-msmtp-pass-available ()
  (interactive)
  (+pass/read-entry (concat "Email/" user-mail-address)))

(use-package! mu4e
  :commands (mu4e mu4e-compose-new)
  :init
  (provide 'html2text)                  ; disable obsolete package
  (setq mu4e-maildir "~/.mail"
        user-mail-address "sarg@sarg.org.ru"
        mu4e-attachment-dir "~/Downloads"
        mu4e-user-mail-address-list nil)
  :config

  (advice-add 'sendmail-send-it
            :before #'sarg/ensure-msmtp-pass-available)

  (setq mu4e-update-interval nil
        mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain"
        mu4e-compose-format-flowed t ; visual-line-mode + auto-fill upon sending
        mu4e-view-show-addresses t
        ;; try to show images
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        ;; iso date format
        mu4e-headers-date-format "%F"
        ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
        mu4e-sent-messages-behavior 'delete
        mu4e-get-mail-command "mbsync -a -c ~/.mbsyncrc"

        ;; mbsync goes crazy without this setting
        mu4e-change-filenames-when-moving t

        ;; Configure sending mail.
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"
        user-full-name "Sergey Trofimov"

        ;; Use the correct account context when sending mail based on the from header.
        message-sendmail-envelope-from 'header

        ;; send with msmtp
        send-mail-function 'sendmail-send-it

        ;; start with the first (default) context;
        mu4e-context-policy 'pick-first
        ;; compose with the current context, or ask
        mu4e-compose-context-policy 'ask-if-none
        ;; use helm/ivy
        mu4e-completing-read-function
        (cond ((featurep! :completion ivy) #'ivy-completing-read)
              ((featurep! :completion helm) #'completing-read)
              (t #'ido-completing-read))
        ;; close message after sending it
        message-kill-buffer-on-exit t
        ;; no need to ask
        mu4e-confirm-quit nil
        ;; remove 'lists' column
        mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 4)
          (:from . 25)
          (:subject))

        ;; bookmarks
        mu4e-bookmarks '(("flag:unread AND NOT flag:trashed AND maildir:/gmail/Inbox" "Gmail messages" ?u)
                         ("date:today..now AND NOT flag:trashed AND NOT maildir:/gmail/trash" "Today's messages" 116)))

  ;; Refresh the current view after marks are executed
  (defun +email*refresh (&rest _) (mu4e-headers-rerun-search))
  (advice-add #'mu4e-mark-execute-all :after #'+email*refresh)
  (advice-add #'mu4e-update-mail-and-index :after #'+email*refresh)

  (when (featurep! :feature spellcheck)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode))

  ;; Wrap text in messages
  (add-hook! 'mu4e-view-mode-hook
    (setq-local truncate-lines nil))

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  (set-evil-initial-state!
    '(mu4e-main-mode
      mu4e-view-mode
      mu4e-headers-mode
      mu4e-compose-mode
      mu4e~update-mail-mode)
    'normal)

  (map! :map mu4e-headers-mode-map
        :n "gR" (lambda () (interactive) (mu4e-update-mail-and-index t)))

  (add-hook 'message-send-mail-hook 'choose-msmtp-account))
