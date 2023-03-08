;;; modules/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+elfeed")
(load! "+dired")
(load! "+email")
(load! "+org")
(load! "+pass")
(load! "+aria2")
;; (load! "+slack")
(load! "+selfcontrol")

(setq +modeline-encoding nil
      +modeline-height 20)

;; debug icons in modeline, find out their names
;; (advice-add '+modeline-format-icon :override
;;             (lambda (icon label &optional face &rest args)
;;               (propertize (format "X %s" label)
;;                           'face face)))

;; C-h deletes character backwards
(define-key key-translation-map [?\C-h] [?\C-?])

;; (set-popup-rule! "^\\*Async Shell" :ttl nil)


(defun browse-url-qute-private (url &optional new-window)
  "Make firefox open URL in private-browsing window."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "qutebrowser " url)
           nil
           "qutebrowser"
           (list (concat ":open -p " url)))))

;; (setq browse-url-browser-function
;;       '(("^https?://some.addr.com" . browse-url-qute-private)
;;         ("." . browse-url-default-browser)))

(setq-default
 ;; russian layout on C-\
 default-input-method "russian-computer"

 ;; set browser
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "qutebrowser")

(after! browse-at-remote
  (setq browse-at-remote-prefer-symbolic nil))

(use-package! web-search
  :custom
  (web-search-default-provider "DuckDuckGo"))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)

  :custom
  (nov-variable-pitch nil)
  (nov-text-width t))

(use-package! webpaste)
(use-package! multitran)

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(after! eshell
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (map! :map eshell-mode-map
                    :ni "C-r" #'+eshell/search-history))))

(defun openscad-preview ()
  (interactive)
  (select-window (split-window-right))
  (start-process "openscad" nil "openscad" (buffer-file-name)))

(define-minor-mode eval-autorun-mode
  "`+eval/buffer' after saving buffer"
  :init-value nil
  :global nil
  :lighter " EAR"
  (if eval-autorun-mode
      (add-hook 'after-save-hook '+eval/buffer nil t)
    (remove-hook 'after-save-hook '+eval/buffer t)))

(use-package! eat
  :hook (eshell-load-hook . (eat-eshell-mode eat-eshell-visual-command-mode)))

(use-package! detached
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))
