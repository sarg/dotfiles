(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/storage/data/emacs/bookmarks")
 '(custom-safe-themes t)
 '(safe-local-variable-directories
   '("/storage/devel/ext/nonguix/" "/storage/devel/ext/guix/"
     "/storage/devel/dotfiles/guix/"
     "/home/sarg/devel/dotfiles/emacs/.doom.d/"))
 '(safe-local-variable-values
   '((buffer-read-only . 1) (eval sarg/eval-org-src-block "api"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
