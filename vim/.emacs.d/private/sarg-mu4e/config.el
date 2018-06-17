
(defun mu4e-goto-unread ()
  (interactive)
  ;; (mu4e-update-mail-and-index nil)
  (mu4e-headers-search (mu4e-get-bookmark-query ?u)))

(spacemacs/set-leader-keys "a m" 'mu4e-goto-unread)
(setq mu4e-enable-mode-line nil)

(with-eval-after-load 'mu4e
  ;; override default trash function
  (add-to-list 'mu4e-marks
             '(trash
	             :char ("d" . "▼")
               :prompt "dtrash"
	             :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
               :action mu4e-custom-delete))

  (define-key mu4e-main-mode-map (kbd "u") 'mu4e-update-mail-and-index)
  (define-key mu4e-headers-mode-map (kbd "q") 'mu4e-quit)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

  (setq
   mu4e-maildir "~/.mail"

   mu4e-get-mail-command "fetchnewmail"
   mu4e-update-interval nil

   ;; notification settings
   mu4e-enable-notifications t
   mu4e-enable-mode-line t
   ;; mu4e-html2text-command "html2text -utf8 -nobs -width 72"
   ;; mu4e-html2text-command "w3m -T text/html"

   ;; display images
   mu4e-view-show-images t

   ;; iso date format
   mu4e-headers-date-format "%F"

   ;; column list for Headers view
   mu4e-headers-fields '(
                         (:human-date . 12)
                         (:flags . 6)
                         (:from . 22)
                         (:subject)
                         )

   ;; close sent message buffers
   message-kill-buffer-on-exit t

   ;; pick first context automatically on launch
   mu4e-context-policy               'pick-first
   ;; use current context for new mail
   mu4e-compose-context-policy       'ask-if-none
   mu4e-confirm-quit                 nil

   ;; mbsync goes crazy without this setting
   mu4e-change-filenames-when-moving t

   ;; bookmarks
   mu4e-bookmarks '(("flag:unread AND NOT flag:trashed AND maildir:/gmail/Inbox" "Unread messages" ?u)
                    ("date:today..now AND NOT flag:trashed AND NOT maildir:/gmail/trash" "Today's messages" ?t))


   ;; Configure sending mail.
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "/usr/bin/msmtp"
   user-full-name "Sergey Trofimov"

   ;; Use the correct account context when sending mail based on the from header.
   message-sendmail-envelope-from 'header

   ;; send with msmtp
   send-mail-function 'sendmail-send-it

   ;;store link to message if in header view, not to header query
   org-mu4e-link-query-in-headers-mode nil

   mu4e-contexts
   `(,(make-mu4e-context
       :name "gmail"
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-maildir-matches msg "^/gmail")))
       :vars '(
               ;; local directories, relative to mail root
               (mu4e-sent-folder . "/gmail/sent")
               (mu4e-drafts-folder . "/gmail/drafts")
               (mu4e-trash-folder . "/gmail/trash")
               (mu4e-refile-folder . "/gmail/all")
               ;; account details
               (user-mail-address . "sarg@sarg.org.ru")
               (user-full-name . "Sergey Trofimov")
               (mu4e-user-mail-address-list . ( "sarg@sarg.org.ru" ))
               ;; gmail saves every outgoing message automatically
               (mu4e-sent-messages-behavior . delete)
               (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?j)
                                          ("/gmail/all" . ?a)
                                          ("/gmail/trash" . ?t)))
               (mu4e-headers-skip-duplicates . t)
               (trash-flags . "-N")
               ))
     ,(make-mu4e-context
       :name "srg"
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-maildir-matches msg "^/srg")))
       :vars '(
               ;; local directories, relative to mail root
               (mu4e-sent-folder . "/srg/sent")
               (mu4e-drafts-folder . "/srg/drafts")
               (mu4e-trash-folder . "/srg/trash")
               (mu4e-refile-folder . "/srg/Inbox")
               ;; account details
               (user-mail-address . "trofimovsi@srgroup.ru")
               (user-full-name . "Sergey Trofimov")
               (mu4e-user-mail-address-list . ( "trofimovsi@srgroup.ru" ))
               (mu4e-sent-messages-behavior . sent)
               (trash-flags . "+T-N")))))
  )
