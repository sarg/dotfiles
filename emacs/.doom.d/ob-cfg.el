;; -*- lexical-binding: t; -*-
(require 'rx)
(defun org-babel-execute:cfg (body params)
  (when-let ((fn (alist-get :file params)))
    (with-temp-buffer
      (insert-file-contents-literally fn nil)
      (setf body (buffer-string))))
  (concat
   "(mixed-text-file\n  \"cfg-file\"\n"
   (replace-regexp-in-string
    (rx (group (*? anything))
        (: ?🐜 (group (*? anything)) ?🐜))
    (lambda (e)
      (concat
       (string-join
        (mapcar
         (-cut prin1-to-string <> nil '((escape-newlines . t)))
         (s-lines (match-string 1 e)))
        "\"\\n\"\n")
       (match-string 2 e)))
    (concat body "🐜🐜") t t)
   ")"))
