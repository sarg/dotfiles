(defun ivy-beginning-or-regex (&rest args)
  (interactive)
  (let ((p (point)))
    (move-beginning-of-line 1)
    (when (= p (point))
      (insert "^"))))

(defun ivy-end-or-regex (&rest args)
  (interactive)
  (let ((p (point)))
    (move-end-of-line 1)
    (when (= p (point))
      (insert "$"))))

(defhydra hydra-switch-buffer-filter ()
  "b - WWW, x - X11
"
  ("b" (insert "WWW ") :exit t)
  ("C-b" (insert "WWW ") :exit t)
  ("x" (insert "X11 ") :exit t))
