(use-package! torrent-mode)

(el-patch-feature aria2)
(after! aria2
  (el-patch-defun aria2--list-entries ()
    "Return entries to be displayed in downloads list."
    (let (entries
          (info (append
                 (Active aria2--bin aria2--tell-keys)
                 (Waiting aria2--bin nil nil aria2--tell-keys)
                 (Stopped aria2--bin nil nil aria2--tell-keys)
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

  (mapc (lambda (file) (Post aria2--bin "aria2.addTorrent"
                                (aria2--base64-encode-file file)
                                []
                                (list :dir dest-dir)))
        (dired-get-marked-files)))

(defsubst aria2--entry-Done (e)
  (let ((total (float (string-to-number (alist-get 'length e))))
        (completed (float (string-to-number (alist-get 'completedLength e)))))
    (if (>= 0 total)
        "-"
      (format "%d%%" (* 100.0 (/ completed total))))))

(defvar-local aria2-entry-gid nil)
(defun aria2--list-entry-files ()
  (let* (entries
         (status (GidStatus aria2--bin aria2-entry-gid))
         (dirPrefixLen (1+ (length (alist-get 'dir status))))
         (info (append (GidFiles aria2--bin aria2-entry-gid) nil)))

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

(defun torrent-do-download-selected (dest-dir)
  "Download ARG entries."
  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t))
   'torrent-mode)

  ;; aria2.addTorrent([secret, ]torrent[, uris[, options[, position]]])
  (Post aria2--bin "aria2.addTorrent"
        (aria2--base64-encode-file buffer-file-name)
        []
        `(:select-file ,(seq-map #'car (tablist-get-marked-items))
          :dir ,dest-dir)))

(evil-collection-define-key 'normal 'torrent-mode-map
  "D" 'torrent-do-download-selected
  "d" nil
  "m" 'tablist-mark-forward)

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
