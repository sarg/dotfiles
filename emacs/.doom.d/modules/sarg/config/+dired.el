(use-package! dired-filter
  :config
  (setq-default dired-filter-stack '((omit) (dot-files)))
  (add-hook! dired-mode #'dired-filter-mode))

(use-package! dired-collapse
  :config
  (add-hook! dired-mode #'dired-collapse-mode))

(use-package! dired-avfs)
(use-package! dired-du
  :config

  ;; human readable
  (setq dired-du-size-format t))

(after! dired
  (setq dired-dwim-target t)
  (add-hook! dired-mode (dired-hide-details-mode 1)))

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

  ;; Don't ask if file is too large when it'll be handled by openwith-mode.
  (advice-add 'abort-if-file-too-large :before-until
              (lambda (size op-type filename &rest args)
                (openwith-has-association filename))))
