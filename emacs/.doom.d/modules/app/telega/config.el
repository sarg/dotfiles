(defun sarg/telega-ins--message (msg &optional no-header)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited."
  (if (telega-msg-special-p msg)
      (telega-ins--with-attrs (list :min (- telega-chat-fill-column
                                            (telega-current-column))
                                    :align 'center
                                    :align-symbol "-")
        (telega-ins--content msg))

    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           (sender (telega-msg-sender msg))
           (channel-post-p (plist-get msg :is_channel_post))
           (private-chat-p (eq "chatTypePrivate" (telega--tl-get chat :type :@type)))
           (avatar (if channel-post-p
                       (telega-chat-avatar-image chat)
                     (telega-user-avatar-image sender)))
           (awidth (string-width (plist-get (cdr avatar) :telega-text)))
           (tfaces (list (if (telega-msg-by-me-p msg)
                             'telega-msg-self-title
                           'telega-msg-user-title)))
           ccol)
      (when telega-msg-rainbow-title
        (let ((color (if channel-post-p
                         (telega-chat-color chat)
                       (telega-user-color sender)))
              (lightp (eq (frame-parameter nil 'background-mode) 'light)))
          (push (list :foreground (nth (if lightp 2 0) color)) tfaces)))

      (telega-ins--with-attrs (list :face tfaces)
        (when (not channel-post-p)
          (telega-ins "<" (telega-user--name sender 'short) "> ")))

      (setq ccol (telega-current-column))
      ;; (telega-ins--fwd-info-inline (plist-get msg :forward_info))
      ;; (telega-ins--reply-inline (telega-msg-reply-msg msg))
      ;;
      (telega-ins--column ccol telega-chat-fill-column
        (telega-ins--content msg)
        (telega-ins-prefix "\n"
          (telega-ins--reply-markup msg)))))

  ;; Date/status starts at `telega-chat-fill-column' column
  t)



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

(defun telega-mute-chat (seconds)
  (interactive (list (let* ((time (org-read-date 'with-time 'to-time nil "Mute until: " nil "+1y"))
                            (seconds (round (float-time (time-subtract time (current-time))))))
                       (if (> seconds 0) seconds 0))))

  (telega--setChatNotificationSettings
      telega-chatbuf--chat
    :mute_for seconds
    :show_preview (or (= 0 seconds) :false)))

(defun telega-set-my-name (first last)
  (interactive "sFirst name: \nsLast name: ")
  (telega-server--call (list :@type "setName" :first_name first :last_name last)))

(def-package! telega
  ;; :load-path "~/devel/ext/telega.el"
  :commands (telega ivy-telega-chat-with)
  :config

  (setq telega-inserter-for-msg-button 'sarg/telega-ins--message)

  (advice-add! 'telega-logout :before-while (lambda (&rest r) (y-or-n-p "Really log out from current account?")))

  (set-popup-rule! "^\\\*Telega Root\*"
    :side 'left
    :size 0.25
    :ttl nil
    :quit 'current
    :select t)

  (when (featurep! :completion ivy)
    (load! "+ivy"))

  (setq telega-chat-use-markdown-formatting t)
  (require 'telega-notifications)
  (telega-notifications-mode 1)

  ;; This fixes the issue with closing buffer when it is visible in other window.
  ;; The logic is as follows:
  ;;   kill-this-buffer is advised with doom*switch-to-fallback-buffer-maybe
  ;;   this function delegates to original kill-this-buffer if the buffer isn't doom-real-buffer-p
  ;;   then doom|protect-visible-functions in kill-buffer-query-functions prevents the close.
  ;; So, to fix this make telega.el chat buffers real.
  (add-hook 'telega-chat-mode-hook #'doom|mark-buffer-as-real)

  (when (featurep! :feature evil)
    (define-key telega-msg-button-map "k" nil)
    (evil-define-key* 'normal telega-root-mode-map
      "u" #'telega-filter-undo)))
