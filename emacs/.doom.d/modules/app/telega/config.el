(defun telega-mute-chat (chat-id seconds)
  (interactive (list (plist-get telega-chatbuf--chat :id)
                     (let* ((time (org-read-date 'with-time 'to-time nil "Mute until: " nil "+1y"))
                            (seconds (round (float-time (time-subtract time (current-time))))))
                       (if (> seconds 0) seconds 0))))

  (telega-server--call (list :@type "setNotificationSettings"
                             :scope (list :@type "notificationSettingsScopeChat" :chat_id chat-id)
                             :notification_settings (list :mute_for seconds
                                                          :show_preview (or (= 0 seconds) :json-false)))))

(defun telega-set-my-name (first last)
  (interactive "sFirst name: \nsLast name: ")
  (telega-server--call (list :@type "setName" :first_name first :last_name last)))

(def-package! telega
  ;; :load-path "/home/sarg/devel/ext/telega.el"
  :config

  (set-popup-rule! "^\\\*Telega Root\*"
    :side 'left
    :size 0.25
    :quit t
    :select t)

  (when (featurep! :completion ivy)
    (load! "+ivy"))

  (require 'telega-notifications)
  (telega-notifications-mode 1)

  ;; This fixes the issue with closing buffer when it is visible in other window.
  ;; The logic is as follows:
  ;;   kill-this-buffer is advised with doom*switch-to-fallback-buffer-maybe
  ;;   this function delegates to original kill-this-buffer if the buffer isn't doom-real-buffer-p
  ;;   then doom|protect-visible-functions in kill-buffer-query-functions prevents the close.
  ;; So, to fix this make telega.el chat buffers real.
  (defun doom-telega-buffer-p (buf)
    "Returns non-nil if BUF is a telega buffer."
    (with-current-buffer buf (derived-mode-p 'telega-chat-mode)))

  (cl-pushnew #'doom-telega-buffer-p doom-real-buffer-functions)

  (when (featurep! :feature evil)
    (define-key telega-msg-button-map "k" nil)
    (evil-define-key* 'normal telega-root-mode-map
      "u" #'telega-filter-undo)))
