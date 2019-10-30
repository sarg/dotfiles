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
      (slack-conversations-open team
                                :user-ids (user-ids)))))

(after! alert
  (setq alert-default-style 'notifications)

  (add-hook! slack-mode-hook #'enable-slack-company))


(defun slack-group-mpim-from-list (user-list)
  (interactive)
  (let* ((team (slack-team-select))
         (users (slack-user-details team))
         (user-ids (mapcar #'(lambda (user) (plist-get user :id))
                           (slack-select-multiple #'(lambda (idx) "Select user: ")
                                                  users
                                                  #'(lambda (idx) (nth idx user-list))))))
    (slack-conversations-open team :user-ids (user-ids))))
