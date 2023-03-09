;;; modules/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(define-minor-mode eval-autorun-mode
  "`+eval/buffer' after saving buffer"
  :init-value nil
  :global nil
  :lighter " EAR"
  (if eval-autorun-mode
      (add-hook 'after-save-hook '+eval/buffer nil t)
    (remove-hook 'after-save-hook '+eval/buffer t)))

