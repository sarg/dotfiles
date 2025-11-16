;;; frf.el --- A FreeFeed.net reader -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (promise "1.1") (request "0.3.0") (org "9.2") (s "1.13.0") (dash "2.20.0"))

;;; Commentary:

;; This package implements FreeFeed.net client.

;;; Code:

(require 'promise)
(require 'request)
(require 's)
(require 'dash)
(require 'cl-lib)
(require 'json)
(require 'auth-source)

(defun frf--promise-json (url &rest opts)
  "Return a promise that resolves with JSON from URL. Pass OPTS directly to (request)."
  (promise-new
   (lambda (resolve reject)
     (apply #'request url
            :headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " (auth-source-pass-get 'secret "Api/freefeed.net"))))
            :parser #'json-read
            :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                  (funcall reject  error-thrown)))
            :success (cl-function (lambda (&key data &allow-other-keys)
                                    (funcall resolve data)))
            opts))))

(defun frf--find (id coll)
  "Find element by ID in an alist COLL."
  (when id
    (seq-find
     (lambda (e) (string= (alist-get 'id e) id))
     coll)))

(defun frf--reflow (str)
  "Reflow string STR to fill paragraphs."
  (with-temp-buffer
    (insert str)
    (fill-region (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward-regexp "^\\*" nil t)
      (replace-match "\\*" nil t))
    (buffer-string)))

(defun frf--render-comments (data from count)
  "Render comments from DATA, skipping FROM, taking COUNT."
  (delete-line)
  (let ((m (point-marker)))
    (cl-loop
     with users = (alist-get 'users data)
     with comments = (-> (alist-get 'comments data)
                         (seq-drop from)
                         (seq-take count))
     for cmt across comments
     do (let-alist cmt
          (let ((author (frf--find .createdBy users)))
            (insert "\n** " (alist-get 'username author) "\n"
                    (s-trim .body) "\n"))))
    (goto-char m)))

(defun frf--load-comments (id from count)
  "Load and render comments for post with ID."
  (promise-chain
      (frf--promise-json (format "https://freefeed.net/v4/posts/%s?maxComments=all&maxLikes=" id))
    (then (lambda (result)
            (frf--render-comments result from count)))))

(defun frf--age (ts now)
  "Return human-readable age string for timestamp TS relative to NOW."
  (let* ((tt (-> ts
                 (string-to-number)
                 (/ 1000)
                 (seconds-to-time)))
         (ds (float-time (time-subtract now tt)))
         (dm (/ ds 60))
         (dh (/ dm 60))
         (dd (/ dh 24)))

    (cond
     ((> dd 30) ">1m")
     ((> dd 1) (format "%dd" dd))
     ((> dh 1) (format "%dh" dh))
     (t "now"))))

(defun frf--render (data buf)
  "Render timeline DATA into buffer BUF."
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+STARTUP: overview indent\n")
    (cl-loop
     with users = (alist-get 'users data)
     with now = (current-time)
     with comments = (alist-get 'comments data)
     with attachments = (alist-get 'attachments data)
     for post across (alist-get 'posts data)
     do (let-alist post
          (let* ((user (frf--find .createdBy users))
                 (body (frf--reflow (s-trim .body)))
                 (title (nth 0 (s-lines body)))
                 (bodyRest (s-trim (substring body (length title)))))
            (insert (format "\n* %s%s {%s} [💕%d/✍%d] :%s:\n"
                            (if .isHidden
                                "COMMENT "
                              (format "[[elisp:(frf-hide \"%s\")][hide]] " .id))
                            (if (string-empty-p body) "Media" title)
                            (frf--age .createdAt now)
                            (+ (length .likes) .omittedLikes)
                            (+ (length .comments) .omittedComments)
                            (alist-get 'username user)))

            (insert bodyRest)

            (when (> (length .attachments) 0)
              (unless (string-empty-p body)
                (insert "\n"))
              (insert
               (string-join
                (seq-map
                 (lambda (a)
                   (let-alist (frf--find a attachments)
                     (let* ((http-link (format "https://freefeed.net/v4/attachments/%s/%s?redirect" a .mediaType))
                            (org-link (if (string= "video" .mediaType)
                                          (format "elisp:(mpv-play-url %S)" http-link)
                                        http-link)))
                       (format "[[%s][%s]]" org-link .mediaType))))
                 .attachments)
                " ")
               "\n"))

            (seq-do-indexed
             (lambda (c idx)
               (let* ((cmt (frf--find c comments))
                      (createdBy (alist-get 'createdBy cmt))
                      (author (or (frf--find createdBy users) '((username . "<unknown>")))))
                 (when (and (> .omittedCommentsOffset 0) (= idx .omittedCommentsOffset))
                   (insert (format "\n** [[elisp:(frf--load-comments \"%s\" %d %d)][%d more comments with %d likes]]"
                                   .id .omittedCommentsOffset .omittedComments
                                   .omittedComments .omittedCommentLikes)))
                 (insert "\n** " (alist-get 'username author) "\n"
                         (alist-get 'body cmt))))
             .comments))))
    (org-mode)
    (setq-local browse-url-browser-function #'eww-browse-url)))

(defun frf-timeline (&optional feed)
  "Read Freefeed timeline."
  (interactive)
  (let* ((name (or feed
                   (when current-prefix-arg (read-string "Name: "))
                   "home"))
         (buf (get-buffer-create (format "*Freefeed: %s*" name))))
    (with-current-buffer buf
      (setq-local revert-buffer-function (lambda (&rest _args) (frf-timeline name)))
      (erase-buffer)
      (insert "Loading...")
      (switch-to-buffer buf))

    (promise-chain (frf--promise-json (format "https://freefeed.net/v4/timelines/%s" name))
      (then
       (lambda (result)
         (frf--render result buf)))
      (promise-catch
       (lambda (reason)
         (message "catch error in promise timeline: %s" reason))))))

(defun frf-hide (id)
  "Hide post with ID from the timeline"
  (let ((buf (current-buffer)))
    (promise-chain (frf--promise-json
                    (format "https://freefeed.net/v4/posts/%s/hide" id)
                    :type "POST")
      (then
       (lambda (result)
         (with-current-buffer buf
           (save-excursion
             (goto-char (point-min))
             (when (search-forward id nil t)
               (beginning-of-line)
               (forward-char 2)
               (insert "COMMENT")
               (let ((start (point)))
                 (search-forward "]]")
                 (delete-region start (point))))))))
      (promise-catch
       (lambda (reason)
         (message "catch error in promise timeline: %s" reason))))))

(provide 'frf)
;;; frf.el ends here
