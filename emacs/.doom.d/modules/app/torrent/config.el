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
              :select-file (string-join
                            (seq-map (-compose #'number-to-string #'car) (tablist-get-marked-items))
                            ",")
              :dir (expand-file-name dest-dir)))

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

(defun sarg/aria2-file-at-point (dest-dir)
  "Download selected files to DEST-DIR. Dwim if DEST-DIR is not
provided and then fallback to `aria2-download-directory'."

  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t)))
  (aria2-ensure-started)
  (mapc (lambda (fn) (addTorrent aria2--cc fn :dir (expand-file-name dest-dir)))
        (dired-get-marked-files))

  (pop-to-buffer aria2-list-buffer-name))

(use-package! aria2
  :config
  (load! "aria2-entry")

  (set-popup-rule! aria2-list-buffer-name :ignore t)

  :custom
  (aria2-download-directory (expand-file-name "~/Downloads"))
  (aria2-start-rpc-server t)
  (aria2-add-evil-quirks t)
  (aria2-custom-args '("--rpc-save-upload-metadata=false")))
