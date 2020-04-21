(use-package! dired-filter
  :hook (dired-mode . dired-filter-mode)
  :config

  (put 'dired-filter-stack 'safe-local-variable (lambda (&rest args) 't))
  (setq dired-filter-stack '((omit) (dot-files))))

(use-package! dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package! dired-git-info
  :custom
  (dgi-commit-message-format "%cr\t%s"))

(setq dgi-commit-message-format "%cr\t%s")
(use-package! dired-avfs)
(use-package! dired-du
  :hook (dired-mode . dired-hide-details-mode)

  :config
  ;; human readable
  (setq dired-du-size-format 't))

(after! async
  (dired-async-mode 1))

(defun openwith-has-association (file)
  (-any? (lambda (oa) (string-match (car oa) file)) openwith-associations))

(use-package! openwith
  :hook (after-init-hook . openwith-mode)
  :config

  (setq openwith-associations
        '(("\\.pdf\\'" "zathura" (file))
          ("\\.epub\\'" "llpp" (file))
          ("\\.\\(?:mkv\\|webm\\|avi\\|mp4\\)\\'" "mpv" (file))))
  ;; (openwith-mode)
  ;; Don't ask if file is too large when it'll be handled by openwith-mode.
  (advice-add 'abort-if-file-too-large :before-until
              (lambda (size op-type filename &rest args)
                (openwith-has-association filename))))
