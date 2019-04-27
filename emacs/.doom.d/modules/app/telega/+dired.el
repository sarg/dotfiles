(defun telega-dwim-chatbuf ()
  (get-window-with-predicate
   (lambda (window)
     (with-current-buffer (window-buffer window)
       (eq major-mode 'telega-chat-mode)))))

(defun dired-telega-copy ()
  (when-let*
      ((files (dired-get-marked-files))
       ((= 1 (length files)))
       (file (first files))
       (chatbuf (telega-dwim-chatbuf)))
    (select-window chatbuf)
    (telega-chatbuf-attach-file file)))

(advice-add 'dired-do-copy :before-until
            (lambda (&rest args)
              (and (telega-dwim-chatbuf)
                   (dired-telega-copy)
                   t)))
