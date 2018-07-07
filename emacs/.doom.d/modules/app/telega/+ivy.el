;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (funcall (telega--tl-prop :unread_count) chat))
        (title (telega-chat--title chat 'with-identity))
        (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

    (if (> (+ unread mentions) 0)
        (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
      title)))

;;;###autoload
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

(setq telega-completing-read-function 'ivy-completing-read)