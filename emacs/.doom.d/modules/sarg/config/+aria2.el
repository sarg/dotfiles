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

(use-package! bencode
  :mode ("\\.torrent\\'" . archive-mode)
  :config

  (advice-add 'archive-find-type :before-until
              (lambda ()
                (when (string-match-p "\\.torrent$" (or buffer-file-name (buffer-name)))
                  'torrent))))

(el-patch-feature aria2)
(after! aria2
  (el-patch-defun aria2--list-entries ()
    "Return entries to be displayed in downloads list."
    (let (entries
          (info (append
                 (tellActive aria2--cc aria2--tell-keys)
                 (tellWaiting aria2--cc nil nil aria2--tell-keys)
                 (tellStopped aria2--cc nil nil aria2--tell-keys)
                 nil)))
      (dolist (e info entries)
        (push (list
               (alist-get 'gid e)
               (vector
                (list (aria2--list-entries-File e)     'face 'aria2-file-face (el-patch-add 'action #'aria2-details-at-point))
                (list (aria2--list-entries-Status e)   'face 'aria2-status-face)
                (list (aria2--list-entries-Type e)     'face 'aria2-type-face)
                (list (aria2--list-entries-Done e)     'face 'aria2-done-face)
                (list (aria2--list-entries-Download e) 'face 'aria2-download-face)
                (list (aria2--list-entries-Upload e)   'face 'aria2-upload-face)
                (aria2--list-entries-Size e)
                (list (aria2--list-entries-Err e)      'face 'aria2-error-face)))
              entries)))))

(use-package! aria2
  :disabled
  :config
  (add-to-list 'evil-normal-state-modes 'aria2-mode)

  (set-popup-rule! aria2-list-buffer-name
    :ignore t)

  (set-popup-rule! "^\\*aria2-entry"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)

  (setq aria2-download-directory (expand-file-name "~/Downloads")
        ;; aria2-add-evil-quirks t
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
                                []
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
                    []
                    (list :select-file indexes :dir dest-dir)))))


(defsubst aria2--entry-Done (e)
  (let ((total (float (string-to-number (alist-get 'length e))))
        (completed (float (string-to-number (alist-get 'completedLength e)))))
    (if (>= 0 total)
        "-"
      (format "%d%%" (* 100.0 (/ completed total))))))

(defvar-local aria2-entry-gid nil)
(defun aria2--list-entry-files ()
  (let* (entries
         (status (tellStatus aria2--cc aria2-entry-gid))
         (dirPrefixLen (1+ (length (alist-get 'dir status))))
         (info (append (getFiles aria2--cc aria2-entry-gid) nil)))

    (dolist (e info entries)
      (push (list e (vector
                     (file-size-human-readable (string-to-number (alist-get 'length e)))
                     (aria2--entry-Done e)

                     (list
                      (substring (alist-get 'path e) dirPrefixLen nil)
                      'face (if (string= "false" (alist-get 'selected e))
                                'aria2-done-face 'aria2-error-face)
                      'action #'aria2-details-find-file)))
            entries))))

(defun aria2-details-find-file (button-marker)
  (let ((file (get-text-property button-marker 'tabulated-list-id)))
    (dired-jump nil (alist-get 'path file))))

(defun aria2-details-at-point (button-marker)
  (let ((id (get-text-property button-marker 'tabulated-list-id)))
    (with-current-buffer (pop-to-buffer "*aria2-entry*")
      (aria2-entry-mode)
      (setq aria2-entry-gid id)
      (tabulated-list-revert))))

(defun aria2--entry-refresh ()
  (setq tabulated-list-entries (aria2--list-entry-files)))

(define-derived-mode aria2-entry-mode tabulated-list-mode "Aria2-entry"
  :group 'aria2
  (setq tabulated-list-format [("Size" 8 t) ("Done" 6 t :right-align t) ("File" 80 t)])
  (setq tabulated-list-entries #'aria2--list-entry-files)
  (add-hook 'tabulated-list-revert-hook #'aria2--entry-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1))


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
