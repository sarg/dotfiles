(defun telega-clear-mentions ()
  (interactive)

  (telega-server--send (list :@type "readAllChatMentions"
     :chat_id (plist-get telega-chatbuf--chat :id))))

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
