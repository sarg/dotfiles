(require 'tablist)
(require 'bencode)

(define-derived-mode torrent-mode tablist-mode
  "torrent"
  "Major mode for torrent files."

  (goto-char (point-min))
  (setq tabulated-list-format [("Idx" 4 t) ("Size" 8 t) ("Name" 80 t)]
        tabulated-list-entries
        (let* ((data (bencode-decode-from-buffer))
               (info (plist-get data :info))
               (files (or (plist-get info :files)
                          (list info))))

          (seq-map-indexed
           (lambda (file index)
             (message "%s" file)
             (let* ((size (plist-get file :length))
                    (name (string-join (or (plist-get file :path)
                                           (list (plist-get file :name)))
                                       "/")))

               (list index (vector (format "%03d" index) (file-size-human-readable size) name))))
           files))

        tabulated-list-padding 3
        tabulated-list-sort-key (cons "Idx" nil))

  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))



(add-to-list 'auto-mode-alist (cons "\\.torrent\\'" 'torrent-mode))

(provide 'torrent-mode)
