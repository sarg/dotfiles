(setq org-directory (expand-file-name "~/Sync/org/"))

(after! tramp
  (pushnew! tramp-remote-path 'tramp-own-remote-path))

(defun +counsel-linux-app-format-function (name comment exec)
  (format "% -45s: %s%s"
          (propertize
           (ivy--truncate-string (replace-regexp-in-string "^/.+/" "" exec) 45)
           'face 'counsel-application-name)
          name
          (if comment
              (concat " - " comment)
            "")))
