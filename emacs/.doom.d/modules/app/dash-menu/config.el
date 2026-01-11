;;; app/dash-menu/config.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'transient)

(defvar dash-menu/elfeed-bookmarks nil)

(after! elfeed
  (setq dash-menu/elfeed-bookmarks
        (mapcar
         (lambda (p)
           (plist-put p :filter
                      (byte-compile
                       (elfeed-search-compile-filter
                        (elfeed-search-parse-filter (plist-get p :query))))))
         '((:name "youtube" :key ?y :query "+unread +youtube")
           (:name "text" :key ?t :query "+unread -youtube")))))

(defun dash-menu/elfeed-query-count (filter)
  "Return the number of feeds returned by the QUERY."
  (let* ((count 0))
    (with-elfeed-db-visit (entry feed)
      (when (funcall filter entry feed count)
        (setf count (1+ count))))
    count))

(defun dash-menu/telega (&rest preds)
  (-map-indexed
   (lambda (index chat)
     `(,(if (zerop index) "tt" (format "t%d" (+ 1 index)))
       ,(telega-msg-sender-title-for-completion chat)
       (lambda () (interactive) (telega-chat--pop-to-buffer ',chat))))
   (-take 9 (telega-filter-chats (telega-chats-list) `(and main unread ,@(-keep #'identity preds))))))

(defun dash-menu/dots-heading ()
  (interactive)
  (let* ((cfg (find-file-noselect "~/devel/dotfiles/emacs/.doom.d/config.org"))
         (sel (with-current-buffer cfg (consult-org-heading))))
    (switch-to-buffer cfg)
    (goto-char sel)))

(transient-define-prefix dash-menu ()
  "My Menu"
  :value '("muted")
  :refresh-suffixes 't
  [["RSS"
    :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("ru" "update" (lambda () (interactive)
                          (elfeed-update)
                          (elfeed)))

         ,@(-map
            (-lambda ((&keys :key :query :name :filter))
              (list (concat "r" (char-to-string key))
                    (format "%s (%d)" name (dash-menu/elfeed-query-count filter))
                    (lambda () (interactive) (elfeed-link-open query))))
            dash-menu/elfeed-bookmarks)]))]
   
   ["Mail"
    :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("mu" "update" (lambda () (interactive) (mu4e-update-mail-and-index t)))
         ("ms" "search" consult-mu)
         ("mo" "open maildir" mu4e-search-maildir)
         ,@(if (featurep 'mu4e)
               (-map
                (-lambda ((&keys :key :unread :query :name))
                  (list (concat "m" (char-to-string key))
                        (format "%s (%d)" name unread)
                        (lambda () (interactive) (mu4e-search-bookmark query))))
                (mu4e-query-items 'bookmarks))
             '())]))]

   ["Telega"
    :class transient-column
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("tm" "" "muted" :format " %k %v")
         ("ts" "Saved" telega-saved-messages)
         ("tc" "chat" telega-chat-with)
         ,@(if (featurep 'telega)
               (dash-menu/telega
                (if (-contains? (oref (transient-prefix-object) value) "muted") nil 'unmuted))
             '())]))]

   ["Sys"
    ("ew" "wifi" iwd-manager)
    ("eb" "blue" bluetooth-list-devices)]

   ["Quick"
    ("<menu>" "consult" consult-buffer)]])

(add-hook! 'transient-setup-buffer-hook
  (setq exwm-input-line-mode-passthrough 't))

(add-hook! 'transient-post-exit-hook
  (setq exwm-input-line-mode-passthrough nil))
