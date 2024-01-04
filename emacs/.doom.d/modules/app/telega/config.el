;; -*- lexical-binding:t -*-
;; (defalias 'sarg/telega-ins--message (symbol-function 'telega-ins--message))

(defun sarg/telega-ins--message-header (msg &optional msg-chat msg-sender
                                            addon-inserter)
  "Insert message's MSG header, everything except for message content.
If ADDON-INSERTER function is specified, it is called with one
argument - MSG to insert additional information after header."
  ;; twidth including 10 chars of date and 1 of outgoing status
  (let* ((fwidth (- telega-chat-fill-column (telega-current-column)))
         (twidth (+ 10 1 fwidth))
         (chat (telega-msg-chat msg))
         (sender (telega-msg-sender msg))
         (channel-post-p (plist-get msg :is_channel_post))
         (saved-msgs-p (eq telega--me-id (plist-get msg :chat_id)))
         (private-chat-p (eq 'chatTypePrivate (telega--tl-type (plist-get chat :type))))
         (tfaces (list (if (telega-msg-by-me-p msg)
                           'telega-msg-self-title
                         'telega-msg-user-title))))
    ;; Maybe add some rainbow color to the message title
    (when telega-msg-rainbow-title
      (let* ((colors (if sender
                         (telega-user-color sender)
                       (telega-chat-color chat)))
             (lightp (eq (frame-parameter nil 'background-mode) 'light))
             (foreground (nth (if lightp 0 1) colors)))
        (when foreground
          (push (list :foreground foreground) tfaces))))

    (telega-ins--with-face 'telega-msg-heading
      (telega-ins--with-attrs (list :max twidth :align 'left :elide t)
        (telega-ins--with-attrs (list :face tfaces)

          ;; Message title itself
          (cond
           (saved-msgs-p
            (telega-ins--date (plist-get msg :date)))
           (private-chat-p
            (telega-ins (if (telega-msg-by-me-p msg) "-> " "<- ")))
           ((not channel-post-p)
            (telega-ins "<" (telega-user-title sender 'full-name) "> "))))

        ;; via <bot>
        (let* ((via-bot-user-id (plist-get msg :via_bot_user_id))
               (via-bot (unless (zerop via-bot-user-id)
                          (telega-user--get via-bot-user-id))))
          (when via-bot
            (telega-ins " via ")
            ;; Use custom :action for clickable @bot link
            (telega-ins--button (telega-user-title via-bot 'username)
              'face 'telega-link        ;no button outline please
              :action (lambda (_msg_ignored)
                        (telega-describe-user via-bot)))))

        ;; Resend button in case message sent failed
        ;; Use custom :action to resend message
        (when-let ((send-state (plist-get msg :sending_state)))
          (when (and (eq (telega--tl-type send-state) 'messageSendingStateFailed)
                     (plist-get send-state :can_retry))
            (telega-ins " ")
            (telega-ins--button "RESEND"
              :action 'telega--resendMessage)))

        ;; Maybe pinned message?
        (when (eq (plist-get msg :id)
                  (plist-get chat :pinned_message_id))
          (telega-ins " " telega-symbol-pin))

        (when addon-inserter
          (cl-assert (functionp addon-inserter))
          (funcall addon-inserter msg))))))

(cl-defun sarg/telega-ins--message0 (msg &key no-header addon-header-inserter)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited."
  (unless (telega-msg-special-p msg)
    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           (sender (telega-msg-sender msg))
           (content (plist-get msg :content))
           (content-type (telega--tl-type content))
           (channel-post-p (plist-get msg :is_channel_post)))
      ;; (telega-ins--with-attrs (list :face tfaces)
      ;;   (cond
      ;;    (private-chat-p
      ;;     (telega-ins (if (telega-msg-by-me-p msg) "-> " "<- ")))
      ;;    ((not channel-post-p)
      ;;     (telega-ins "<" (telega-user--name sender 'name) "> "))))
      (telega-ins--with-props
          (list 'action (lambda (button)
                          ;; NOTE: check for custom message :action first
                          ;; - [RESEND] button uses :action
                          ;; - via @bot link uses :action
                          (or (telega-button--action button)
                              (if sender
                                  (telega-describe-user sender)
                                (telega-describe-chat chat)))))
        ;; (telega-ins--image avatar 0
        ;;                    :no-display-if (not telega-chat-show-avatars))
        (sarg/telega-ins--message-header msg addon-header-inserter)
        ;; (telega-ins--image avatar 1
        ;;                    :no-display-if (not telega-chat-show-avatars))
        )

      (telega-ins " ")
      (telega-ins--fwd-info-inline (plist-get msg :forward_info))
      (telega-ins--msg-reply-inline msg)
      (if (memq content-type '(messageSticker))
          (telega-ins "\n"))
      (telega-ins--content msg)

      (when channel-post-p (insert ?\n ?\n ?\^L ?\n)))
    t))

(defun sarg/telega-ins--message (msg &rest args)
  "Inserter for the message MSG."
  (if (telega-msg-marked-p msg)
      (progn
        (telega-ins--line-wrap-prefix (telega-symbol 'mark)
          (apply #'sarg/telega-ins--message0 msg args)))
    (apply #'sarg/telega-ins--message0 msg args)))

(defun sarg/telega-get-code ()
  "Extract confirmation code from latest message of telegram user BenderBot."
  (interactive)
  (let* ((chat (cl-find "BenderBot" telega--ordered-chats
                        :test (lambda (needname chat)
                                (string= (telega-chat-title chat)
                                         needname))))

         (text (telega--tl-get chat :last_message :content :text :text))

         (re-list (list (rx (or "Code: " "Kod " "Код " "код: "
                                "Пароль для подтвердения - ")
                            (group (1+ digit))))))

    (set-text-properties 0 (length text) nil text)
    (when (-any (lambda (re) (string-match re text)) re-list)
      (kill-new (match-string 1 text)))))

(defun telega-set-my-name (first last)
  (interactive "sFirst name: \nsLast name: ")
  (telega-server--call (list :@type "setName" :first_name first :last_name last)))

(use-package! page-break-lines)
(use-package! telega
  :commands (telega ivy-telega-chat-with)

  ;; This fixes the issue with closing buffer when it is visible in other window.
  ;; The logic is as follows:
  ;;   kill-this-buffer is advised with doom*switch-to-fallback-buffer-maybe
  ;;   this function delegates to original kill-this-buffer if the buffer isn't doom-real-buffer-p
  ;;   then doom|protect-visible-functions in kill-buffer-query-functions prevents the close.
  ;; So, to fix this make telega.el chat buffers real.
  :hook (telega-chat-mode . doom-mark-buffer-as-real-h)
  :hook (telega-chat-mode . +telega|init-chatbuf)

  :init
  (setq telega-inserter-for-msg-button #'sarg/telega-ins--message)

  :config
  (add-hook! telega-root-mode (cd telega-directory))

  (setq
   telega-root-show-avatars nil
   telega-chat-show-avatars nil
   telega-chat-input-markups '("markdown2" nil)
   telega-animation-play-inline nil
   telega-sticker-size '(8 . 48)

   telega-chat-folder-format nil

   telega-emoji-custom-alist '((":s:" . "¯\\_(ツ)_/¯")))

  (advice-add #'telega-ins--chat-sponsored-message :override #'ignore)

  (setq telega-hide-previews 't)
  ;; show previews for photo/video webpages
  (advice-add #'telega-ins--webpage :before-while
              (lambda (msg &rest args)
                (let ((ht (telega--tl-get msg :content :web_page :type)))
                  (and telega-hide-previews (-contains? '("video" "photo") ht)))))

  ;; insert just content of a message, no headers needed
  ;; -> message text
  ;;
  ;; instead of
  ;;
  ;; XX Username
  ;; XX message text                                                    08.04.20✓
  (advice-add #'telega-msg--pp
              :override
              (apply-partially #'telega-button--insert 'telega-msg))

  (defun +telega|init-chatbuf ()
    (cd telega-directory)
    (setq-local visual-fill-column-width (+ 11 telega-chat-fill-column))
    (setq-local visual-fill-column-center-text nil)
    (visual-line-mode)
    (visual-fill-column-mode)
    (page-break-lines-mode))

  (advice-add
   'telega-logout
   :before-while (lambda (&rest r) (y-or-n-p "Really log out from current account?")))

  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :ignore t)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'left
    :size 0.25
    :ttl nil
    :quit t
    :select t)

  (when (modulep! :completion ivy)
    (load! "+ivy"))

  (after! dired
    (load "telega-dired-dwim.el"))

  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
           "k" nil
           "l" nil))))
