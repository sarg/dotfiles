;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (plist-get chat :unread_count))
        (title (telega-chat-title chat 'with-username))
        (not-muted-p (not (telega-chat-muted-p chat)))
        (mentions (plist-get chat :unread_mention_count)))

    (if (and not-muted-p (> (+ unread mentions) 0))
        (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
      title)))

(defun ivy-translit-re-builder (s)
  (let* ((fr "qwertyuiop[]asdfghjkl;\\zxcvbnm,.`")
         (to "йцукенгшщзхъфывапролджэячсмитьбюё")
         (trans-table
          (-map (lambda (c)
                  (cons (car c)
                        (format "[%s%s]" (car c) (cdr c))))
                (-zip (split-string fr "" 't)
                      (split-string to "" 't)))))

    (s-replace-all trans-table s)))


;;;###autoload
(defun ivy-telega-chat-with ()
  "Starts chat with defined peer"
  (interactive)

  (telega t)
  (let ((chats (mapcar
                (lambda (x) (cons (ivy-telega-chat-highlight x) x))
                (telega-filter-chats telega--ordered-chats 'all))))
    (ivy-read "chat: " chats
              :action (lambda (x) (telega-chat--pop-to-buffer (cdr x)))
              :re-builder #'ivy-translit-re-builder
              :caller 'ivy-telega-chat-with
              :require-match 't)))

(setq telega-completing-read-function 'ivy-completing-read)
(after! ivy-prescient
  (add-to-list 'ivy-prescient-sort-commands
               'telega-msg-forward-marked-or-at-point
               'append))
