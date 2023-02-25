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

(map!
 "C-x b" #'bufler-switch-buffer

 (:map dired-mode-map
   :n (kbd "DEL") #'dired-up-directory)

 (:map ivy-switch-buffer-map
   "C-c C-k" #'ivy-switch-buffer-kill)

 (:map image-mode-map
   :n "q" #'kill-current-buffer)

 (:map dired-mode-map
   :n "K" #'dired-do-kill-lines)
 (:when (featurep! :completion ivy)
   :after ivy
   :map ivy-minibuffer-map
   "C-b" #'hydra-switch-buffer-filter/body
   "C-e" #'ivy-end-or-regex
   "C-a" #'ivy-beginning-or-regex))
