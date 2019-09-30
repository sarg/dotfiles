(use-package! dired-filter
  :hook (dired-mode . dired-filter-mode)
  :custom
  (dired-filter-stack '((omit) (dot-files))))

(use-package! dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(use-package! dired-avfs)
(use-package! dired-du
  :hook (dired-mode . dired-hide-details-mode)

  :custom
  ;; human readable
  (dired-dwim-target t)
  (dired-du-size-format t))

(after! async
  (dired-async-mode 1))

(defun openwith-has-association (file)
  (-any? (lambda (oa) (string-match (car oa) file)) openwith-associations))

(use-package! openwith
  :custom
  (openwith-associations
   '(("\\.pdf\\'" "zathura" (file))
     ("\\.epub\\'" "llpp" (file))
     ("\\.\\(?:mkv\\|webm\\|avi\\|mp4\\)\\'" "mpv" (file))))

  :config
  (openwith-mode)
  ;; Don't ask if file is too large when it'll be handled by openwith-mode.
  (advice-add 'abort-if-file-too-large :before-until
              (lambda (size op-type filename &rest args)
                (openwith-has-association filename))))
