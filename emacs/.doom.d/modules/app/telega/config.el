(defun telega-get--date (timestamp)
  "Insert DATE.
Format is:
- HH:MM      if today
- Mon/Tue/.. if on this week
- DD.MM.YY   otherwise"
  (let* ((dtime (decode-time timestamp))
         (current-ts (time-to-seconds (current-time)))
         (ctime (decode-time current-ts))
         (today00 (telega--time-at00 current-ts ctime)))
    (if (> timestamp today00)
        (format "%02d:%02d" (nth 2 dtime) (nth 1 dtime))

      (let* ((week-day (nth 6 ctime))
             (mdays (+ week-day
                       (- (if (< week-day telega-week-start-day) 7 0)
                          telega-week-start-day)))
             (week-start00 (telega--time-at00
                            (- current-ts (* mdays 24 3600)))))
        (if (> timestamp week-start00)
            (nth (nth 6 dtime) telega-week-day-names)

          (format "%02d.%02d.%02d"
                  (nth 3 dtime) (nth 4 dtime) (- (nth 5 dtime) 2000))))
      )))

(defun sarg/telega-ins--message (msg &optional no-header)
  "Insert message MSG.
If NO-HEADER is non-nil, then do not display message header
unless message is edited."
  (unless (telega-msg-special-p msg)
    ;; Message header needed
    (let* ((chat (telega-msg-chat msg))
           (sender (telega-msg-sender msg))
           (channel-post-p (plist-get msg :is_channel_post))
           (private-chat-p (eq 'chatTypePrivate (telega--tl-type (plist-get chat :type))))
           (content (plist-get msg :content))
           (content-type (telega--tl-type content))
           (avatar (if channel-post-p
                       (telega-chat-avatar-image chat)
                     (telega-user-avatar-image sender)))
           (tfaces (list (if (telega-msg-by-me-p msg)
                             'telega-msg-self-title
                           'telega-msg-user-title)))
           ccol)
      ;; #17161c
      ;;

      (when telega-msg-rainbow-title
        (let ((color (if channel-post-p
                         (telega-chat-color chat)
                       (telega-user-color sender)))
              (lightp (eq (frame-parameter nil 'background-mode) 'light)))
          (push (list :foreground (nth (if lightp 2 0) color)) tfaces)))

      (telega-ins--with-attrs (list :face tfaces)
        (cond
         (private-chat-p
          (telega-ins (if (telega-msg-by-me-p msg) "-> " "<- ")))
         ((not channel-post-p)
          (telega-ins "<" (telega-user--name sender 'name) "> "))))

      (setq ccol (telega-current-column))
      (telega-ins--fwd-info-inline (plist-get msg :forward_info))
      (telega-ins--msg-reply-inline msg)

      (if (memq content-type '(messagePhoto messageSticker))
          (progn
            (telega-ins "\n  ")
            (telega-ins--content msg))

        (telega-ins--content msg)

        (telega-ins-prefix "\n"
          (telega-ins--reply-markup msg)))

      (when channel-post-p (insert ?\n ?\n ?\^L ?\n)))
    (telega-ins "\n")
    t))

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

  :custom
  (telega-inserter-for-msg-button 'sarg/telega-ins--message)
  (telega-chat-use-markdown-formatting t)
  (telega-server-libs-prefix "/usr")
  (telega-msg-group-by-sender t)

  :config

  (defun telega-msg--pp (msg)
    "Pretty printer for MSG button."
    (telega-button--insert 'telega-msg msg))

  (defun +telega|init-chatbuf ()
    (setq-local visual-fill-column-width (+ 11 telega-chat-fill-column))
    (setq-local visual-fill-column-center-text nil)
    (visual-line-mode)
    (visual-fill-column-mode)
    (page-break-lines-mode))

  (advice-add
   'telega-logout
   :before-while (lambda (&rest r) (y-or-n-p "Really log out from current account?")))

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'left
    :size 0.25
    :ttl nil
    :quit t
    :select t)

  (when (featurep! :completion ivy)
    (load! "+ivy"))

  (after! dired
    (load! "+dired"))

  (after! sauron
    (load! "sauron-telega")
    (add-to-list 'sauron-modules 'sauron-telega)
    (sauron-telega-start))

  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
       "k" nil
       "l" nil))))
