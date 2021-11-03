;; (defun +default-inhibit-authinfo-for-sudo-a (orig-fn &rest args)
;;   (apply orig-fn args))

(setq org-directory (expand-file-name "~/Sync/org/"))
;; (defalias 'fit-frame-to-buffer-1 #'fit-frame-to-buffer)

(setq shell-command-switch "-c"
      shell-file-name "bash-emacs")

(after! tramp
  (pushnew! tramp-remote-path 'tramp-own-remote-path))

(defun sarg/connect-bt ()
  (interactive)
  (bluetooth--call-method
   "dev_B8_AD_3E_42_00_34"
   :device
   #'dbus-call-method-asynchronously
   "Connect"
   (lambda () (notifications-notify :title "bt connected"))))

(use-package! bluetooth)
(defun +counsel-linux-app-format-function (name comment exec)
  (format "% -45s: %s%s"
          (propertize
           (ivy--truncate-string (replace-regexp-in-string "^/.+/" "" exec) 45)
           'face 'counsel-application-name)
          name
          (if comment
              (concat " - " comment)
            "")))

(use-package! calibredb
  :config
  (setq calibredb-root-dir (expand-file-name "~/Calibre Library"))
  (setq calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
  (setq calibredb-library-alist '(("~/Calibre Library"))))
