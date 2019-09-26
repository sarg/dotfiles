(defun telega-dwim-chatbuf ()
  (get-window-with-predicate
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (eq major-mode 'telega-chat-mode)))))

(defun dired-telega-copy ()
  (when-let*
      ((files (seq-filter #'file-regular-p (dired-get-marked-files)))
       (chatbuf (telega-dwim-chatbuf)))
    (select-window chatbuf)
    (mapc #'telega-chatbuf-attach-file files)))

(advice-add 'dired-do-copy :before-until
            (lambda (&rest args)
              (and (telega-dwim-chatbuf)
                   (dired-telega-copy)
                   t)))
