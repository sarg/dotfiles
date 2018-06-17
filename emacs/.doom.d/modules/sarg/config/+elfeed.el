(after! elfeed

  (advice-add 'elfeed-update :around
              (lambda (orig-fun &rest args)
                (if (or (= 0 (mod
                              (calendar-day-of-week (calendar-current-date))
                              6)) ; 0 and 6 - Sunday and Saturday
                        (> (nth 2 (decode-time)) 18))
                    (apply orig-fun args)
                  (message "Time for rss did not come yet!")))
              '(name "restrict-rss-by-time"))


  (setq elfeed-search-filter "@2-week-ago +unread ")
  (require 'evil-collection-elfeed)
  (evil-collection-elfeed-setup)

  (setq elfeed-show-entry-switch (lambda (buf) (display-buffer-below-selected buf nil)))

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
    "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

  (defun ambrevar/elfeed-visit-or-play-with-mpv ()
    "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise.
See `elfeed-play-with-mpv'."
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

  (evil-define-key 'normal elfeed-show-mode-map
    "go" 'ambrevar/elfeed-visit-or-play-with-mpv
  ))
