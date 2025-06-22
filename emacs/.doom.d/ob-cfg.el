;; -*- lexical-binding: t; -*-
(require 'rx)
(defun org-babel-execute:cfg (body params)
  (when-let* ((fn (alist-get :file params)))
    (with-temp-buffer
      (insert-file-contents-literally fn nil)
      (setf body (buffer-string))))
  (concat
   "(mixed-text-file\n  \"cfg-file\"\n"
   (replace-regexp-in-string
    (rx (group (*? anything))
        (: ?🐜 (group (*? anything)) ?🐜))
    (lambda (e)
      (let ((string-part (match-string 1 e))
            (scheme-part (match-string 2 e)))
        (concat
         (if (string-empty-p string-part) ""
           (string-join
            (mapcar
             (lambda (l) (prin1-to-string l nil '((escape-newlines . t))))
             (string-lines string-part nil t))
            "\n"))
         scheme-part)))
    (concat body "🐜🐜") t t)
   ")"))
