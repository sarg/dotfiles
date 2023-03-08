(set-popup-rule! "^\\*aria2-entry"
  :size 0.75 :actions '(display-buffer-below-selected)
  :select t :quit nil :ttl t)

(defadvice! +aria2-with-entry-mode (entries)
  "Add action to the name button."
  :filter-return #'aria2--list-entries
  (mapc (lambda (el)
          (push 'action (cdr (last (aref (cadr el) 0))))
          (push #'aria2-details-at-point (cdr (last (aref (cadr el) 0)))))
        entries))

(defun aria2-details-at-point (button-marker)
  (let ((id (get-text-property button-marker 'tabulated-list-id)))
    (with-current-buffer (pop-to-buffer "*aria2-entry*")
      (aria2-entry-mode)
      (setq aria2-entry-gid id)
      (tabulated-list-revert))))

;; defuns for aria2-entry mode
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
