;;; -*- lexical-binding: t; -*-

(defun aria2-download (fn &rest selected)
  (require 'aria2)
  (let ((dir (read-directory-name "Directory: " aria2-download-directory nil t)))
    (aria2-downloads-list)
    (addTorrent aria2--cc fn
                :select-file (mapconcat (lambda (idx) (number-to-string (1+ idx)))
                                        selected ",")
                :dir (expand-file-name dir))))

(use-package! torrent-mode
  :custom
  (torrent-mode-download-function #'aria2-download))

(defun aria2-notify-completion (entry)
  (notifications-notify
   :title "Download complete"
   :app-name "aria2"
   :body (aria2--list-entries-File entry)))

(use-package! aria2
  :defer t
  :config
  (set-popup-rule! aria2-list-buffer-name :ignore t)

  ;; upstream it
  (when (modulep! :editor evil +everywhere)
    (evil-collection-inhibit-insert-state 'aria2-mode-map)
    (evil-set-initial-state 'aria2-mode-map 'normal)
    (evil-collection-define-key 'normal 'aria2-mode-map
      "Q" 'aria2-terminate
      "p" 'aria2-toggle-pause
      "a" 'aria2-add
      "D" 'aria2-remove-download
      "C" 'aria2-clean-removed-download
      (kbd "M-k") 'aria2-move-up-in-list
      (kbd "M-j") 'aria2-move-down-in-list))

  :hook
  (aria2-on-complete . aria2-notify-completion)

  :custom
  (aria2-download-directory (or (xdg-user-dir "DOWNLOAD")
                                (expand-file-name "~")))
  (aria2-start-rpc-server t)
  (aria2-custom-args '("--rpc-save-upload-metadata=false")))
