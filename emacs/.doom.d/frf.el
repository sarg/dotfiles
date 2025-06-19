;;; frf.el --- A Friendfeed reader -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (promise "1.1") (request "0.3.0") (org "9.2") (s "1.13.1") (dash "2.20.0"))

(require 'promise)
(require 'request)
(require 's)
(require 'dash)

(defun frf--promise-json (url)
  (promise-new
   (lambda (resolve reject)
     (request url
       :headers
       `(("Content-Type" . "application/json")
         ("Authorization" . ,(concat "Bearer " (auth-source-pass-get 'secret "Api/freefeed.net"))))
       :parser #'json-read
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (funcall reject  error-thrown)))
       :success (cl-function (lambda (&key data &allow-other-keys)
                               (funcall resolve data)))))))

(defun frf-find (id coll)
  (seq-find
   (lambda (e) (string= (alist-get 'id e) id))
   coll))

(defun frf-reflow (str)
  (with-temp-buffer
    (insert str)
    (fill-region (point-min) (point-max))
    (buffer-string)))

(defun frf-render-comments (data from count)
  (delete-line)
  (let ((m (point-marker)))
    (cl-loop
     with users = (alist-get 'users data)
     with comments = (-> (alist-get 'comments data)
                         (seq-drop from)
                         (seq-take count))
     for cmt across comments
     do (let-alist cmt
          (let ((author (frf-find .createdBy users)))
            (insert "\n** [" (alist-get 'username author) "]\n"
                    (s-trim .body) "\n"))))
    (goto-char m)))

(defun frf-load-comments (id from count)
  (promise-chain
      (frf--promise-json (format "https://freefeed.net/v4/posts/%s?maxComments=all&maxLikes=" id))
    (then (lambda (result)
            (frf-render-comments result from count)))))

(defun frf-render (data buf)
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+STARTUP: overview indent\n")
    (cl-loop
     with users = (alist-get 'users data)
     with comments = (alist-get 'comments data)
     with attachments = (alist-get 'attachments data)
     for post across (alist-get 'posts data)
     do (let-alist post
          (let* ((user (frf-find .createdBy users))
                 (body (frf-reflow (s-trim .body)))
                 (title (nth 0 (s-lines body)))
                 (bodyRest (s-trim (substring body (length title)))))
            (insert "\n* "
                    (if (string-empty-p body) "Media" title)
                    (format " [💕%d/✍%d] :%s:\n"
                            (length .likes)
                            (+ (length .comments) .omittedComments)
                            (alist-get 'username user)))

            (insert bodyRest)

            (when (> (length .attachments) 0)
              (unless (string-empty-p body)
                (insert "\n"))
              (seq-do
               (lambda (a)
                 (let-alist (frf-find a attachments)
                   (insert "[[https://freefeed.net/v4/attachments/" a "/" .mediaType "?"
                           (url-build-query-string `((redirect) (fn ,.fileName)))
                           "][" .mediaType "]] ")))
               .attachments)
              (insert "\n"))

            (seq-do-indexed
             (lambda (c idx)
               (let* ((cmt (frf-find c comments))
                      (createdBy (alist-get 'createdBy cmt))
                      (author (frf-find createdBy users)))
                 (when (and (> .omittedCommentsOffset 0) (= idx .omittedCommentsOffset))
                   (insert (format "\n** [[elisp:(frf-load-comments \"%s\" %d %d)][%d more comments with %d likes]]"
                                   .id .omittedCommentsOffset .omittedComments
                                   .omittedComments .omittedCommentLikes)))
                 (insert "\n** [" (alist-get 'username author) "]\n"
                         (alist-get 'body cmt))))
             .comments))))
    (org-mode)))


(defun frf-timeline (name)
  (interactive (list (read-string "Name: " nil nil "home")))
  (let ((buf (get-buffer-create "*Freefeed*")))
    (with-current-buffer buf
      (setq-local browse-url-browser-function #'eww-browse-url)
      (erase-buffer)
      (insert "Loading...")
      (switch-to-buffer buf))

    (promise-chain (frf--promise-json (format "https://freefeed.net/v4/timelines/%s" name))
      (then (lambda (result)
              (frf-render result buf)))
      (promise-catch (lambda (reason)
                       (message "catch error in promise timeline: %s" reason))))))

(provide 'frf)
;;; frf.el ends here
