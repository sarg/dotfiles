;;; app/dash-menu/config.el -*- lexical-binding: t; -*-

(require 'mu4e)
(require 'mu4e-query-items)
(require 'mu4e-search)
(require 'dash)
(require 'telega-match)
(require 'transient)
(require 'elfeed)

(defvar dash-menu/elfeed-bookmarks
  '((:name "youtube" :query "+unread +youtube" :key ?y)
    (:name "text" :query "+unread -youtube" :key ?t)))

(defun dash-menu/elfeed-query-count (query)
  "Return the number of feeds returned by the QUERY."
  (let* ((count 0)
         (filter (elfeed-search-parse-filter query))
         (func (byte-compile (elfeed-search-compile-filter filter))))
    (with-elfeed-db-visit (entry feed)
      (when (funcall func entry feed count)
        (setf count (1+ count))))
    count))

(defmacro dash-menu/mu4e-bookmarks ()
  '(-map
    (-lambda ((&keys :key :unread :query :name))
      (list (concat "m" (char-to-string key))
            (format "%s (%d)" name unread)
            (lambda () (interactive) (mu4e-search-bookmark query))))
    (mu4e-query-items 'bookmarks)))

(defmacro dash-menu/elfeed-bookmarks ()
  '(-map
    (-lambda ((&keys :key :query :name))
      (list (concat "r" (char-to-string key))
            (format "%s (%d)" name (dash-menu/elfeed-query-count query))
            (lambda () (interactive) (elfeed-link-open query))))
    dash-menu/elfeed-bookmarks))

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
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("ru" "update" (lambda () (interactive)
                          (elfeed-update)
                          (elfeed)))
         ,@(dash-menu/elfeed-bookmarks)]))]

   ["Mail"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("mu" "update" (lambda () (interactive) (mu4e-update-mail-and-index t)))
         ("ms" "search" consult-mu)
         ("mo" "open maildir" mu4e-search-maildir)
         ,@(dash-menu/mu4e-bookmarks)]))]

   ["Telega"
    :setup-children
    (lambda (_)
      (transient-parse-suffixes
       'dash-menu
       `[("tm" "" "muted" :format " %k %v")
         ("ts" "Saved" telega-saved-messages)
         ("tc" "chat" telega-chat-with)
         ,@(dash-menu/telega
            (if (-contains? (oref (transient-prefix-object) value) "muted") nil 'unmuted))]))]

   ["Quick"
    ("<menu>" "bookmarks" consult-bookmark)]])

(add-hook! 'transient-setup-buffer-hook
  (setq exwm-input-line-mode-passthrough 't))

(add-hook! 'transient-post-exit-hook
  (setq exwm-input-line-mode-passthrough nil))
