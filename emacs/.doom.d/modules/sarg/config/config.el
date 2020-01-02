;;; private/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+elfeed")
(load! "+dired")
(load! "+email")
(load! "+org")
(load! "+pass")
(load! "+aria2")
(load! "+slack")
(load! "+selfcontrol")

(after! doom-modeline
  (setq doom-modeline-height 20
        doom-modeline-icon t
        doom-modeline-percent-position nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-encoding nil)

  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))

;; C-h deletes character backwards
(define-key key-translation-map [?\C-h] [?\C-?])

(set-popup-rule! "^\\*Async Shell" :ttl nil)

;; russian layout on C-\
(setq-default
 default-input-method "russian-computer")

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

(use-package! white-sand-theme)
(setq doom-theme 'white-sand
      doom-font (font-spec :family "Hack" :size 16))

(use-package! web-search
  :custom
  (web-search-default-provider "DuckDuckGo"))

(use-package! nov
  :custom
  (nov-variable-pitch nil)
  (nov-text-width t)

  :mode
  ("\\.epub\\'" . nov-mode))

(use-package! ox-hugo
  :after org)
(use-package! puppet-mode)
(use-package! nginx-mode)

(use-package! webpaste)
;; (use-package! ranger
;;   :config
;;   (ranger-override-dired-mode t))

(use-package! multitran)
(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(after! eshell
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (map! :map eshell-mode-map
                    :ni "C-r" #'counsel-esh-history)
              (setq eshell-visual-commands
                    (cons eshell-visual-commands '("htop" "less" "ssh" "docker" "docker-compose"))))))

(after! plantuml
  (setq plantuml-default-exec-mode 'jar
        plantuml-java-args '("-Djava.awt.headless=true" "-jar"))

  (set-popup-rule! "^\\*PLANTUML" :size 0.4 :select nil :ttl 0 :side 'right))
