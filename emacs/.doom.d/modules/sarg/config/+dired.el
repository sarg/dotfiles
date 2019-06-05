(def-package! dired-filter
  :config
  (setq-default dired-filter-stack '((omit) (dot-files)))
  (add-hook! dired-mode #'dired-filter-mode))

(def-package! dired-collapse
  :config
  (add-hook! dired-mode #'dired-collapse-mode))

(def-package! dired-avfs)
(def-package! dired-du)

(after! dired
  (setq dired-dwim-target t))

(after! async
  (dired-async-mode 1))

(def-package! openwith
  :config
  (setq openwith-associations
        '(("\\.pdf\\'" "zathura" (file))
          ("\\.epub\\'" "llpp" (file))
          ("\\.\\(?:mkv\\|avi\\|mp4\\)\\'" "mpv" (file))))

  (openwith-mode t))
