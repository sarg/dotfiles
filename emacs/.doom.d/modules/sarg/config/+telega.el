(use-package telega
  :load-path  "~/devel/ext/telega.el"
  :defer t
  :commands (telega telega-filter-chats ivy-telega-chat-with)
  :config
  (defun ivy-telega-chat-highlight (chat)
    (let ((unread (funcall (telega--tl-prop :unread_count) chat))
          (title (telega-chat--title chat 'with-identity))
          (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

      (if (> (+ unread mentions) 0)
          (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
        title)))

  (defun ivy-telega-chat-with ()
    "Starts chat with defined peer"
    (interactive)
    (unless (process-live-p (telega-server--proc)) (telega-server--start))
    (let ((chats (mapcar
                  (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                  (telega-filter-chats 'all))))
      (ivy-read "chat: " chats
                :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
                :caller 'ivy-telega-chat-with)))

  (set-popup-rule! "^\\\*Telega Root\*"
    '((size . 0.25) (side . left))
    '((quit . current) (select . t)))

  (when (featurep! :completion ivy)
    (setq telega-completing-read-function 'ivy-completing-read))

  (evil-define-key* 'normal telega-root-mode-map
    "u" #'telega-filter-undo))

(after! exwm
  (exwm-input-set-key (kbd "s-i") #'ivy-telega-chat-with))

(use-package telega-notifications
  :load-path  "~/devel/ext/telega.el"
  :after (telega)
  :config (add-hook 'telega-root-mode-hook (lambda () (telega-notifications-mode 1))))

