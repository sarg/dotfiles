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

(def-package! mu4e
  :commands (mu4e mu4e-compose-new)
  :init
  (setq mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/Downloads"
        mu4e-user-mail-address-list nil)
  :config
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
        mu4e-get-mail-command "fetchnewmail"

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
        mu4e-completing-read-function (cond ((featurep! :completion ivy) #'ivy-completing-read)
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

  (add-hook 'message-send-mail-hook 'choose-msmtp-account)

  (after! evil
    (cl-loop for str in '((mu4e-main-mode . normal)
                          (mu4e-view-mode . normal)
                          (mu4e-headers-mode . normal)
                          (mu4e-compose-mode . normal)
                          (mu4e~update-mail-mode . normal))
             do (evil-set-initial-state (car str) (cdr str)))

    (setq mu4e-view-mode-map (make-sparse-keymap)
          ;; mu4e-compose-mode-map (make-sparse-keymap)
          mu4e-headers-mode-map (make-sparse-keymap)
          mu4e-main-mode-map (make-sparse-keymap))

    (map! (:map (mu4e-main-mode-map mu4e-view-mode-map)
            :leader
            :n "," #'mu4e-context-switch
            :n "." #'mu4e-headers-search-bookmark
            :n ">" #'mu4e-headers-search-bookmark-edit
            :n "/" #'mu4e~headers-jump-to-maildir)

          (:map (mu4e-headers-mode-map mu4e-view-mode-map)
            :localleader
            :n "f" #'mu4e-compose-forward
            :n "r" #'mu4e-compose-reply
            :n "c" #'mu4e-compose-new
            :n "e" #'mu4e-compose-edit)

          (:map mu4e-main-mode-map
            :n "q"   #'mu4e-quit
            :n "u"   #'mu4e-update-mail-and-index
            :n "J"   #'mu4e~headers-jump-to-maildir
            :n "c"   #'+email/compose
            :n "b"   #'mu4e-headers-search-bookmark)

          (:map mu4e-headers-mode-map
            :n "q"   #'mu4e-quit
            :n "r"   #'mu4e-compose-reply
            :n "c"   #'mu4e-compose-edit
            :n "s"   #'mu4e-headers-search-edit
            :n "S"   #'mu4e-headers-search-narrow
            :n "RET" #'mu4e-headers-view-message
            :n "u"   #'mu4e-headers-mark-for-unmark
            :n "U"   #'mu4e-mark-unmark-all
            :n "v"   #'evil-visual-line
            :nv "d"  #'+email/mark
            :nv "="  #'+email/mark
            :nv "-"  #'+email/mark
            :nv "+"  #'+email/mark
            :nv "!"  #'+email/mark
            :nv "?"  #'+email/mark
            :nv "r"  #'+email/mark
            :nv "m"  #'+email/mark
            :n "x"   #'mu4e-mark-execute-all

            :n "]]"  #'mu4e-headers-next-unread
            :n "[["  #'mu4e-headers-prev-unread

            (:localleader
              :n "u" 'mu4e-update-mail-and-index
              :n "s" 'mu4e-headers-change-sorting
              :n "t" 'mu4e-headers-toggle-threading
              :n "r" 'mu4e-headers-toggle-include-related

              :n "%" #'mu4e-headers-mark-pattern
              :n "t" #'mu4e-headers-mark-subthread
              :n "T" #'mu4e-headers-mark-thread))

          (:map mu4e-view-mode-map
            :n "q" #'mu4e~view-quit-buffer
            :n "r" #'mu4e-compose-reply
            :n "c" #'mu4e-compose-edit

            :n "<M-Left>"  #'mu4e-view-headers-prev
            :n "<M-Right>" #'mu4e-view-headers-next
            :n "[m" #'mu4e-view-headers-prev
            :n "]m" #'mu4e-view-headers-next
            :n "[u" #'mu4e-view-headers-prev-unread
            :n "]u" #'mu4e-view-headers-next-unread

            (:localleader
              :n "%" #'mu4e-view-mark-pattern
              :n "t" #'mu4e-view-mark-subthread
              :n "T" #'mu4e-view-mark-thread

              :n "d" #'mu4e-view-mark-for-trash
              :n "r" #'mu4e-view-mark-for-refile
              :n "m" #'mu4e-view-mark-for-move))

          (:map mu4e~update-mail-mode-map
            :n "q" #'mu4e-interrupt-update-mail))))


(def-package! org-mu4e
  :commands org-mu4e-compose-org-mode
  :init (add-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  :config
  (setq org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t)

  ;; Only render to html once. If the first send fails for whatever reason,
  ;; org-mu4e would do so each time you try again.
  (add-hook! 'message-send-hook
    (setq-local org-mu4e-convert-to-html nil)))

