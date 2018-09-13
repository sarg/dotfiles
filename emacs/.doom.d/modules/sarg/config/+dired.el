(def-package! dired-filter
  :config
  (add-hook! dired-mode #'dired-filter-mode))

(def-package! dired-collapse
  :config
  (add-hook! dired-mode #'dired-collapse-mode))
