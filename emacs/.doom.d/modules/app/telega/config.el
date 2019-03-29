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
