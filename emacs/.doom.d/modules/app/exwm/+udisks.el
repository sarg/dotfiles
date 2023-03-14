(use-package! discomfort
  :config
  (add-to-list 'evil-emacs-state-modes 'discomfort-mode)
  (define-key discomfort-mode-map
              "j" #'next-line
              "k" #'previous-line))
