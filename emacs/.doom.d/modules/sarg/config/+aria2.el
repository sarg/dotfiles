(defun archive-torrent-summarize()
  (let* ((data (bencode-decode-from-buffer))
         (retfiles '())
         (info (plist-get data :info))
         (files (or (plist-get info :files)
                    (list info))))

    (read-only-mode)
    (goto-char (point-min))
    (insert (format "M     Size  Name") "\n")

    (archive-summarize-files
     (seq-map-indexed (lambda (file index)
               (let* ((size (plist-get file :length))
                      (name (string-join (or (plist-get file :path)
                                             (list (plist-get file :name)))
                                         "/"))
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
                (when (string-match-p "\\.torrent$" (or buffer-file-name (buffer-name)))
                  'torrent))))

(use-package aria2
  :config

  (set-popup-rule! aria2-list-buffer-name
    :ignore t)

  (setq aria2-download-directory (expand-file-name "~/Downloads")
        aria2-add-evil-quirks t
        aria2-custom-args '("--rpc-save-upload-metadata=false")))

(defun sarg/aria2-file-at-point (dest-dir)
  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t)))

  (mapc (lambda (file) (make-request aria2--cc "aria2.addTorrent"
                                (aria2--base64-encode-file file)
                                (vector)
                                (list :dir dest-dir)))
        (dired-get-marked-files)))

(defun sarg/aria2-marked-files (dest-dir)
  "Download selected files in torrent with aria2 to DEST-DIR."
  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t)))

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


;; ; Replace continuous spans of integers in sorted array with '(spanStart . spanEnd)
;; (assert (equal '((1 . 2) (5 . 7) 10 13)
;; (let* ((arr '(1 2 5 6 7 10 13))
;;       (spanStart (first arr))
;;       (spanEnd spanStart)
;;       (state)
;;       (ret '()))

;;   (case state
;;     ('init (setq spanStart el
;;                  spanEnd el
;;                  state 'span))
;;     ('span (if (< (- el spanEnd) 2)
;;                (setq spanEnd el)
;;              (setq state 'print
;;                    spanStart el
;;                    spanEnd el))))

;;   (when (eq state 'print)

;;     )

;;   (-each arr (lambda (el)
;;             (if (< (- el spanEnd) 2)
;;                 (setq spanEnd el)
;;               (push (if (= spanEnd spanStart) spanEnd (cons spanStart spanEnd)) ret)
;;               (setq spanStart el)
;;               (setq spanEnd el))))
;;   (push (if (= spanEnd spanStart) spanEnd (cons spanStart spanEnd)) ret)
;;   (nreverse ret))))
