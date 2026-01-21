;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun torrent-do-download-selected (dest-dir)
  "Download ARG entries."
  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t))
   torrent-mode)

  (aria2-ensure-started)
  (addTorrent aria2--cc original-buffer-file-name
              :select-file
              (substring-no-properties
               (string-join
                (seq-map
                 (lambda (el) (aref (cdr el) 0))
                 (tablist-get-marked-items))
                ","))

              :dir (expand-file-name dest-dir)))


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

(use-package! torrent-mode
  :mode ("\\.torrent\\'" . 'torrent-mode)

  :config
  (after! evil-collection
    (evil-collection-define-key 'normal 'torrent-mode-map
      "D" 'torrent-do-download-selected
      "d" nil
      "m" 'tablist-mark-forward)))

(defun aria2-ensure-started ()
  "Ensure aria2 is up and running."
  (with-current-buffer (get-buffer-create aria2-list-buffer-name)
    (aria2-mode)))

(use-package! aria2
  :config
  (load! "aria2-entry")

  (set-popup-rule! aria2-list-buffer-name :ignore t)

  :custom
  (aria2-download-directory (expand-file-name "~/Downloads"))
  (aria2-start-rpc-server t)
  (aria2-add-evil-quirks t)
  (aria2-custom-args '("--rpc-save-upload-metadata=false")))
