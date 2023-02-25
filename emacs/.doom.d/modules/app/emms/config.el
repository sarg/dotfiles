(defun dbus-capable ()
  "Check if dbus is available"
  (unwind-protect
      (let (retval)
        (condition-case ex
            (setq retval (dbus-ping :session "org.freedesktop.Notifications"))
          ('error
           (message (format "Error: %s - No dbus" ex))))
        retval)))

(when (modulep! :app emms +spotify)
  (if (dbus-capable)
      (load! "+spotify")))

(use-package! emms
  :config

  (emms-all)
  (emms-history-load)
  (setq emms-player-list (list emms-player-mpv)
        emms-playlist-buffer-name "*Music*"
        emms-source-file-default-directory (expand-file-name "~/Music")
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-browser-covers 'emms-browser-cache-thumbnail))
