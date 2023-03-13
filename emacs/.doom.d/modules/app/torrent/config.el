(use-package! torrent-mode
  :mode ("\\.torrent\\'" . 'torrent-mode)

  :config
  (defun torrent-do-download-selected (dest-dir)
    "Download ARG entries."
    (interactive
     (list (read-directory-name "Directory: "
                                (or (dired-dwim-target-directory)
                                    aria2-download-directory)
                                nil
                                t))
     'torrent-mode)

    ;; aria2.addTorrent([secret, ]torrent[, uris[, options[, position]]])
    (make-request aria2--cc "aria2.addTorrent"
                  (aria2--base64-encode-file buffer-file-name)
                  []
                  `(:select-file ,(seq-map #'car (tablist-get-marked-items)) :dir ,(expand-file-name dest-dir))))

  (after! evil-collection
    (evil-collection-define-key 'normal 'torrent-mode-map
      "D" 'torrent-do-download-selected
      "d" nil
      "m" 'tablist-mark-forward)))

(defun sarg/aria2-file-at-point (dest-dir)
  "Download selected files to DEST-DIR. Dwim if DEST-DIR is not
provided and then fallback to `aria2-download-directory'."

  (interactive
   (list (read-directory-name "Directory: "
                              (or (dired-dwim-target-directory)
                                  aria2-download-directory)
                              nil
                              t)))

  (mapc (apply-partially 'addTorrent aria2--cc) (dired-get-marked-files)))

(use-package! aria2
  :config
  (load! "aria2-entry")

  (set-popup-rule! aria2-list-buffer-name :ignore t)

  :custom
  (aria2-download-directory (expand-file-name "~/Downloads"))
  (aria2-start-rpc-server t)
  (aria2-add-evil-quirks t)
  (aria2-custom-args '("--rpc-save-upload-metadata=false")))
