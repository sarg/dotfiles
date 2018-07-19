(def-package! dired-filter
  :config
  (add-hook! dired-mode-hook #'dired-filter-mode))

(def-package! dired-collapse
  :config
  (add-hook! dired-mode-hook #'dired-collapse-mode))
