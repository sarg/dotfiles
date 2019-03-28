(defun archive-torrent-summarize()
  (let* ((data (bencode-decode-from-buffer))
         (retfiles '())
         (files (plist-get (plist-get data :info) :files)))

    (read-only-mode)
    (goto-char (point-min))
    (insert (format "M     Size  Name") "\n")

    (archive-summarize-files
     (seq-map-indexed (lambda (file index)
               (let* ((size (plist-get file :length))
                      (name (string-join (plist-get file :path) "/"))
                      (text (format "  %8s  %s" (file-size-human-readable size) name)))

                 ;; [EXT-FILE-NAME INT-FILE-NAME CASE-FIDDLED MODE ...]")
                 (push (vector name name nil nil (1+ index)) retfiles)
                 (vector text 2 (length text)))) files))

    (apply #'vector (nreverse retfiles))))

(use-package bencode
  :config
  (push (cons "\\.torrent$" 'archive-mode) auto-mode-alist)

  (advice-add 'archive-find-type :before-until
              (lambda ()
                (and
                 (string-match "\\.torrent" (or buffer-file-name (buffer-name)))
                 'torrent))))

(use-package aria2
  :config

  (setq aria2-download-directory (expand-file-name "~/Downloads")))

(defun sarg/aria2-marked-files (dest-dir)
  "Download selected files in torrent with aria2 to DEST-DIR."
  (interactive
   (list
    (read-directory-name "Directory: "
                         (or (dired-dwim-target-directory)
                              aria2-download-directory)
                         nil
                         t)))

  (set-popup-rule! aria2-list-buffer-name
    :ignore t)

  (let (indexes base64-file)
    (setq indexes (string-join
                   (mapcar (lambda (f) (number-to-string (aref f 4))) (archive-get-marked ?*))
                   ","))

    (save-restriction
      (widen)

      ;; aria2.addTorrent([secret, ]torrent[, uris[, options[, position]]])
      (make-request aria2--cc "aria2.addTorrent"
                    (base64-encode-string
                     (buffer-substring archive-proper-file-start (1+ (buffer-size))) t)
                    (vector)
                    (list :select-file indexes :dir dest-dir)))))
