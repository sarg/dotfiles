;;; private/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+elfeed")
(load! "+dired")
(load! "+email")
(load! "+org")
(load! "+pass")
(load! "+aria2")
;; (load! "+slack")
(load! "+selfcontrol")

;; (after! doom-modeline
;;   (setq doom-modeline-height 20
;;         doom-modeline-icon t
;;         doom-modeline-percent-position nil
;;         doom-modeline-major-mode-icon nil
;;         doom-modeline-buffer-encoding nil)

;;   (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))
;;
(setq +modeline-encoding nil
      +modeline-height 20)

;; debug icons in modeline, find out their names
;; (advice-add '+modeline-format-icon :override
;;             (lambda (icon label &optional face &rest args)
;;               (propertize (format "X %s" label)
;;                           'face face)))

;; C-h deletes character backwards
(define-key key-translation-map [?\C-h] [?\C-?])

(setq lsp-keymap-prefix nil)
(set-popup-rule! "^\\*Async Shell" :ttl nil)

;; russian layout on C-\
(setq-default
 default-input-method "russian-computer")


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

;; set browser
(setq-default
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "qutebrowser")

(defun oleh-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(setq sarg-repos-dir (expand-file-name "~/devel/ext"))
(after! magit
  (setq magit-repository-directories `((,sarg-repos-dir . 1))
        magit-clone-default-directory `,sarg-repos-dir))

(after! browse-at-remote
  (setq browse-at-remote-prefer-symbolic nil))

(use-package! white-sand-theme)
(defvar doom-theme-dark 'kaolin-valley-dark)
(setq doom-theme 'white-sand
      doom-font (font-spec :family "Hack" :size 20)
      doom-serif-font (font-spec :family "Hack")
      doom-unicode-font (font-spec :family "Hack"))

(use-package! web-search
  :custom
  (web-search-default-provider "DuckDuckGo"))

(use-package! nov
  :custom
  (nov-variable-pitch nil)
  (nov-text-width t)

  :mode
  ("\\.epub\\'" . nov-mode))

;; (use-package! puppet-mode)
;; (use-package! nginx-mode)

(use-package! webpaste)
;; (use-package! ranger
;;   :config
;;   (ranger-override-dired-mode t))

(use-package! multitran
  :config

  (defadvice! multitran-fix-useragent (orig-fn &rest args)
    :around #'multitran--url
    (let ((url-privacy-level 'paranoid))
      (apply orig-fn args))))

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(after! eshell
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (map! :map eshell-mode-map
                    :ni "C-r" #'+eshell/search-history))))

(after! ivy-rich
  (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)

  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-rich-candidate
                  (:width (lambda (x)
                            (if (eq 'exwm-mode (ivy-rich--local-values x 'major-mode))
                                x
                              (ivy-rich-normalize-width x 30)))))
                 (ivy-rich-switch-buffer-path
                  (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))))

  ;; reload to make new display-transformer work
  (ivy-rich-reload))

(after! plantuml
  (setq plantuml-default-exec-mode 'jar
        plantuml-java-args '("-Djava.awt.headless=true" "-jar"))

  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0 :side 'right))


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

(use-package! bluetooth)

(use-package! calibredb
  :config
  (setq calibredb-root-dir (expand-file-name "~/Calibre Library"))
  (setq calibredb-db-dir (concat calibredb-root-dir "/metadata.db"))
  (setq calibredb-library-alist '(("~/Calibre Library"))))

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
