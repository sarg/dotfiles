(use-package! wpa-manager
  :init

  (when (modulep! :editor evil +everywhere)
    (load! "+evil")))
