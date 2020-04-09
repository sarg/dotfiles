(defun enable-slack-company ()
  (interactive)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-slack-backend))

(defun slack-user-details-to-string (user team)
  (let* ((profile (slack-user-profile user))
         (header (slack-user-header user team))
         (email (plist-get profile :email)))
    (format "%s%s" header (if email (concat " (" email ")") ""))))

(defun slack-user-details (team &optional filter)
  "Return all users as alist (\"user-name\" . user) in TEAM."
  (let ((users (cl-remove-if #'slack-user-hidden-p
                             (slack-team-users team))))
    (mapcar (lambda (u) (cons (slack-user-details-to-string u team) u))
            (if (functionp filter)
                (funcall filter users)
              users))))

(defun slack-group-mpim-open+ ()
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-details team)))
    (cl-labels
        ((prompt (loop-count)
                 (if (< 0 loop-count)
                     "Select another user (or leave empty): "
                   "Select user: "))
         (user-ids ()
                   (mapcar #'(lambda (user) (plist-get user :id))
                           (slack-select-multiple #'prompt users))))
      (slack-conversations-open team :user-ids user-ids))))

(after! alert
  (setq alert-default-style 'notifications))

(defun slack-user-select+ ()
  "Select user from team, then display the user's profile."
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-details team))
         (selected (slack-select-from-list (users "Select User: ")))
         (user-id (plist-get selected :id))
         (im (slack-im-find-by-user-id user-id team)))
    (if im
        (slack-room-display im team)
      (slack-conversations-open team :user-ids (list user-id)))))


(defun slack-group-mpim-from-list (user-list)
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-details team))
         (user-ids (mapcar #'(lambda (user) (plist-get user :id))
                           (slack-select-multiple #'(lambda (idx) "Select user: ")
                                                  users
                                                  #'(lambda (idx) (nth idx user-list))))))
    (slack-conversations-open team :user-ids user-ids)))

(defun slack-insert-image-from-clipboard ()
  (interactive)
  (let* ((buffer slack-current-buffer)
         (team (slack-buffer-team buffer))
         (room (slack-buffer-room buffer))
         (file (make-temp-file "clip" nil ".png"))
         (selection-coding-system 'no-conversion) ;for rawdata
         (coding-system-for-write 'binary))

    (write-region (or (gui-get-selection 'CLIPBOARD 'image/png)
                      (error "No image in CLIPBOARD"))
                  nil file nil 'quiet)

    (cl-labels
        ((on-file-upload (&key data &allow-other-keys)
                         (slack-request-handle-error
                          (data "slack-file-upload"))))

      (slack-request
       (slack-request-create
        slack-file-upload-url
        team
        :type "POST"
        :params (list
                 (cons "channels" (oref room id))
                 '("filename" . "image.png")
                 '("filetype" . "png")
                 (if-let ((initial-comment (read-from-minibuffer "Message: ")))
                     (cons "initial_comment" initial-comment)))
        :files (list (cons "file" file))
        :headers (list (cons "Content-Type" "multipart/form-data"))
        :success #'on-file-upload)))))

(use-package! slack
  :hook (slack-mode-hook . enable-slack-company)

  :init
  (set-popup-rule! "^\\*Slack" :ignore t)
  (setq slack-prefer-current-team t)
  (map!
   :map slack-message-buffer-mode-map
   "C-c C-a" #'slack-clipboard-image-upload
   "C-c C-v" #'slack-insert-image-from-clipboard)

  :commands (slack-start))
