(defun ambrevar/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be")
  "List of regexp to match against elfeed entry link to know whether to use mpv to visit the link.")

(defun ambrevar/elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise. See `elfeed-play-with-mpv'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-mpv-patterns))
    (while (and patterns (not (string-match (car elfeed-mpv-patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (ambrevar/elfeed-play-with-mpv)
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))))

(defun sarg/elfeed-strip-content (entry)
  (when (find "rmbody" (elfeed-entry-tags entry))
    (setf (elfeed-entry-content entry) (elfeed-ref ""))
    (elfeed-untag entry "rmbody")))

(after! elfeed
  (add-hook 'elfeed-new-entry-hook #'sarg/elfeed-strip-content)
  (add-hook 'elfeed-show-mode-hook (lambda () (setq-local browse-url-generic-program "qutebrowser-background")))

  (setq elfeed-search-filter "@2-week-ago +unread -youtube"
        elfeed-show-entry-switch (lambda (buf) (display-buffer-below-selected buf nil) (select-window (get-buffer-window buf))))

  (evil-define-key 'normal elfeed-show-mode-map
    "go" 'ambrevar/elfeed-visit-or-play-with-mpv
  ))
