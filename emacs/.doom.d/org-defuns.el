;; https://isamert.net/2022/01/04/dealing-with-apis-jsons-and-databases-in-org-mode.html
(defun org-babel-execute:json (body params)
  (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))

(defun sarg/eval-org-src-block (name)
  "Eval elisp block NAME. To be used in org files in Local variables section."

  (save-excursion
    (org-babel-goto-named-src-block name)
    (let* ((block-info (org-babel-get-src-block-info))
           (lang (nth 0 block-info))
           (body (nth 1 block-info)))
      (if (or (string= lang "emacs-lisp")
              (string= lang "elisp"))
          (cl-loop with next = 0
                   with maxlen = (length body)
                   for (sexp . next) = (read-from-string body next)
                   do (eval sexp)
                   while (< next maxlen))

        (error "%s is not an emacs-lisp src block" name)))))
