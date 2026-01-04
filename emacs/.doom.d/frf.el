;;; frf.el --- A FreeFeed.net reader -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (promise "1.1") (request "0.3.0") (org "9.2") (s "1.13.0") (dash "2.20.0"))

;;; Commentary:

;; This package implements FreeFeed.net client.

;;; Code:

(require 'promise)
(require 'request)
(require 'browse-url)
(require 's)
(require 'org)
(require 'dash)
(require 'cl-lib)
(require 'json)
(require 'auth-source)

(defvar-local frf-feed "home"
  "Feed name.")

(defvar-local frf-offset 0
  "Offset to read messages.")

(defun frf--promise-json (path &rest opts)
  "Return a promise that resolves with JSON from Freefeed's API endpoint at PATH.
Pass OPTS directly to (request)."
  (promise-new
   (lambda (resolve reject)
     (apply #'request (concat "https://freefeed.net/v4" path)
            :headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " (auth-source-pass-get 'secret "Api/freefeed.net"))))
            :encoding 'utf-8
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
          (insert "\n** "
                  (if (> .likes 0) (format "[%s%d] " (if (eq t .hasOwnLike) "🫶" "💕") .likes) "")
                  (alist-get 'username (frf--find .createdBy users)) "\n"
                  ":PROPERTIES:\n"
                  ":commentId: " .id "\n"
                  (if (eq t .hasOwnLike) ":liked: t\n" "")
                  ":END:\n"
                  (s-trim .body) "\n")))
    (goto-char m)))

(defun frf--load-comments (id &rest args)
  "Load comments for post with ID. Pass them and the ARGS to the render function."
  (promise-chain
      (frf--promise-json (format "/posts/%s?maxComments=all&maxLikes=" id))
    (then (lambda (result)
            (apply #'frf--render-comments result args)))))

(defun frf--age (ts now)
  "Return human-readable age string for timestamp TS relative to NOW."
  (let* ((tt (-> ts
                 (string-to-number)
                 (/ 1000)
                 (seconds-to-time)))
         (ds (float-time (time-subtract now tt)))
         (dm (/ ds 60))
         (dh (/ dm 60))
         (dd (round (/ dh 24)))
         (dmon (/ dd 30))
         (dy (/ dd 365))
         (dym (/ (mod dd 365) 30)))

    (cond
     ((and (> dy 1) (> dym 0)) (format "%dy%dm" dy dym))
     ((> dy 1) (format "%dy" dy))
     ((> dmon 1) (format "%dm" dmon))
     ((> dd 1) (format "%dd" dd))
     ((> dh 1) (format "%dh" dh))
     (t "now"))))

(defun frf-like ()
  "Like thing at point."
  (interactive)

  (let ((commentId (org-entry-get nil "commentId"))
        (postId (org-entry-get nil "postId" t)))
    (promise-chain
        (frf--promise-json
         (if commentId
             (concat "/comments/" commentId
                     (if (org-entry-get nil "liked") "/unlike" "/like"))
           (concat "/posts/" postId "/like"))
         :type "POST" :data "{}")
      (then
       (lambda (_) (message "Liked")))
      (promise-catch
       (lambda (reason) (message "Failed: %s" reason))))))

(defun frf-send-comment ()
  "Send current comment."
  (interactive)

  (let* ((body (substring-no-properties (org-get-entry)))
         (postId (org-entry-get nil "postId" t))
         (req `((comment . ((body . ,body) (postId . ,postId))))))
    (promise-chain
        (frf--promise-json "/comments" :type "POST" :data (json-encode req))
      (then
       (lambda (_) (message "Comment sent")))
      (promise-catch
       (lambda (reason) (message "Failed: %s" reason))))))

(defun frf--render (data buf)
  "Render timeline DATA into buffer BUF."
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert "#+STARTUP: overview indent\n")
    (insert "Feed: " frf-feed)
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
                            (if .isHidden "DONE " "")
                            (if (string-empty-p body) "Media" title)
                            (frf--age .createdAt now)
                            (+ (length .likes) .omittedLikes)
                            (+ (length .comments) .omittedComments)
                            (alist-get 'username user)))

            (insert ":PROPERTIES:\n"
                    ":postId: " .id "\n"
                    ":END:\n")
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
                      (likes (alist-get 'likes cmt))
                      (ownLike (eq t (alist-get 'hasOwnLike cmt)))
                      (author (or (frf--find createdBy users) '((username . "<unknown>")))))
                 (when (and (> .omittedCommentsOffset 0) (= idx .omittedCommentsOffset))
                   (insert (format "\n** [[elisp:(frf--load-comments \"%s\" %d %d)][%d more comments with %d likes]]"
                                   .id .omittedCommentsOffset .omittedComments
                                   .omittedComments .omittedCommentLikes)))
                 (insert "\n** "
                         (if (> likes 0) (format "[%s%d] " (if ownLike "🫶" "💕") likes) "")
                         (alist-get 'username author) "\n"
                         ":PROPERTIES:\n"
                         ":commentId: " (alist-get 'id cmt) "\n"
                         (if ownLike ":liked: t\n" "")
                         ":END:\n"
                         (frf--reflow (alist-get 'body cmt)))))
             .comments))))

    (insert "\n* ")
    (when (> frf-offset 0)
      (insert (format "[[elisp:(frf-timeline \"%s\" %d)][Newer]] " frf-feed (- frf-offset 30))))
    (insert (format "[[elisp:(frf-timeline \"%s\" %d)][Older]]" frf-feed (+ frf-offset 30)))
    (org-mode)
    (add-hook 'org-after-todo-state-change-hook
              (lambda ()
                (when (equal org-state "DONE")
                  (frf-hide (org-entry-get nil "postId"))))
              nil t)
    (add-to-list 'browse-url-handlers
                 (cons "media.freefeed.net/attachments/" #'eww-browse-url))
    (add-to-list 'browse-url-handlers
                 (cons "freefeed.net/v4/attachments/" #'eww-browse-url))))

(defun frf-timeline (&optional feed offset)
  "Read Freefeed's FEED timeline from OFFSET."
  (interactive)
  (let* ((name (or feed (read-string "Name: " "home")))
         (buf (get-buffer-create (format "*Freefeed: %s*" name))))
    (with-current-buffer buf
      (setq-local frf-offset (or offset 0)
                  frf-feed name
                  revert-buffer-function (lambda (&rest _args) (frf-timeline name frf-offset)))
      (keymap-local-set "C-c C-m" #'frf-send-comment)
      (keymap-local-set "C-c C-l" #'frf-like)
      (erase-buffer)
      (insert "Loading...")
      (switch-to-buffer buf))

    (promise-chain (frf--promise-json
                    (format
                     (if (s-starts-with? "?" name) "/search?qs=%s&offset=%d" "/timelines/%s?offset=%d")
                     (url-hexify-string (s-chop-prefix "?" name))
                     frf-offset))
      (then
       (lambda (result)
         (frf--render result buf)))
      (promise-catch
       (lambda (reason)
         (message "catch error in promise timeline: %s" reason))))))

(defun frf-hide (id)
  "Hide post with ID from the timeline."
  (let ((buf (current-buffer)))
    (promise-chain
        (frf--promise-json (format "/posts/%s/hide" id) :type "POST")
      (then (lambda (_) (message "post hidden")))
      (promise-catch
       (lambda (reason)
         (with-current-buffer buf
           (save-excursion
             (goto-char (point-min))
             (when (search-forward id nil t)
               (org-todo ""))))

         (message "hide failed: %s" reason))))))

(provide 'frf)
;;; frf.el ends here
