;; -*- lexical-binding: t; -*-
(require 'rx)
(defun org-babel-execute:cfg (body params)
  "Wrap BODY in a (mixed-text-file). If PARAMS contains :file, use its
contents instead of BODY. If PARAMS contain :tangle-mode, additionally
wrap with (chmod-computed-file)."
  (let* ((fn (alist-get :file params))
         (tangle-mode (alist-get :tangle-mode params))
         (perms (and tangle-mode (org-babel-interpret-file-mode tangle-mode))))
    (when fn
      (with-temp-buffer
        (insert-file-contents-literally fn nil)
        (setf body (buffer-string))))

    (concat
     (if perms "(chmod-computed-file " "")
     "(mixed-text-file\n  \"cfg-file\"\n"
     ;; split string into parts 'string-part🐜scheme-part🐜'
     ;; - output each line of 'string-part' quoted.
     ;; - output 'scheme-part' as is
     ;; - note: add an empty 'scheme-part' at the end of input string
     ;;   so that the last 'string-part' is matched
     (replace-regexp-in-string
      (rx (minimal-match
           (group (zero-or-more anything))
           ?🐜 (group (zero-or-more anything)) ?🐜))
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
     ")"
     (if perms (format " #o%o)" perms) ""))))
