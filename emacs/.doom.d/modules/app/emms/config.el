(when (featurep! :app emms +spotify)
  (load! "+spotify"))

(when (featurep! :app emms +volume)
  (load! "+volume"))

(use-package! emms
  :config

  (emms-all)
  (emms-history-load)
  (setq emms-player-list (list emms-player-mpv)
        emms-playlist-buffer-name "*Music*"
        emms-source-file-default-directory (expand-file-name "~/Music")
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-browser-covers 'emms-browser-cache-thumbnail))
