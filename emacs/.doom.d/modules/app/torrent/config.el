;;; -*- lexical-binding: t; -*-


(defvar aria2-notify-completion-timer nil
  "Timer object for `aria2-notify-completion-mode` to check aria2 status.")

(defun aria2-notify-completion-check ()
  (when (or (not aria2--cc)
            (and
             (seq-empty-p (tellActive aria2--cc aria2--tell-keys))
             (seq-empty-p (tellWaiting aria2--cc nil nil aria2--tell-keys))))
    
    (setq mode-line-process '(:propertize ":🏁" face gnus-summary-high-read))
    (notifications-notify :title "aria2" :body "All downloads complete")
    (cancel-timer aria2-notify-completion-timer)))

(define-minor-mode aria2-notify-completion-mode
  "Notify when aria2 downloads complete."
  :global nil
  :group 'aria2
  :ligher "🏁"
  
  (cond
   ((not (and (eq major-mode 'aria2-mode)
              aria2--cc))
    (error "Choose aria2 buffer first."))
   
   (aria2-notify-completion-mode
    (setq mode-line-process '(:propertize ":⚐" face gnus-summary-high-read))
    (setq aria2-notify-completion-timer
          (run-with-timer 0 10 #'aria2-notify-completion-check)))

   (t
    (when aria2-notify-completion-timer
      (cancel-timer aria2-notify-completion-timer)
      (setq mode-line-process nil)
      (setq aria2-notify-completion-timer nil)))))


(defun aria2-format-select-file (files)
  (mapconcat (lambda (idx) (number-to-string (1+ idx))) files ","))

(defun aria2-download (fn &rest selected)
  (require 'aria2)
  (let ((dir (read-directory-name "Directory: " aria2-download-directory nil t)))
    (aria2-downloads-list)
    (addTorrent aria2--cc fn
                :select-file (aria2-format-select-file selected)
                :dir (expand-file-name dir))))

(use-package! torrent-mode
  :custom
  (torrent-mode-download-function #'aria2-download))

(use-package! aria2
  :defer t
  :config
  (set-popup-rule! aria2-list-buffer-name :ignore t)

  ;; upstream it
  (evil-collection-inhibit-insert-state 'aria2-mode-map)
  (evil-set-initial-state 'aria2-mode-map 'normal)
  (evil-collection-define-key 'normal 'aria2-mode-map
    "Q" 'aria2-terminate
    "p" 'aria2-toggle-pause
    "a" 'aria2-add
    "D" 'aria2-remove-download
    "C" 'aria2-clean-removed-download
    (kbd "M-k") 'aria2-move-up-in-list
    (kbd "M-j") 'aria2-move-down-in-list)

  :custom
  (aria2-download-directory (expand-file-name "~/Downloads"))
  (aria2-start-rpc-server t)
  (aria2-custom-args '("--rpc-save-upload-metadata=false")))
