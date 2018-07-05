;;; private/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+elfeed")
(load! "+email")
(load! "+org")

;; (after! dired
;;   (require 'evil-collection-dired)
;;   (evil-collection-dired-setup)
;;   (evil-define-key 'normal dired-mode-map
;;     (kbd doom-leader-key) nil))

;; (after! eshell
;;   (require 'evil-collection-eshell)
;;   (evil-collection-eshell-setup))

;; (after! term
;;   (require 'evil-collection-term)
;;   (evil-collection-term-setup))

;; C-h deletes character backwards
(define-key key-translation-map [?\C-h] [?\C-?])

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

(setq doom-theme 'doom-one-light
      doom-font (font-spec :family "Hack" :size 14))

(after! prodigy
  (prodigy-define-service
   :name "nginx"
   :command "docker-compose"
   :args '("up")
   :cwd "~/devel/nginx")
  (prodigy-define-service
   :name "redis"
   :command "docker"
   :args '("start" "-a" "redis")))

(def-package! ox-hugo)
(def-package! puppet-mode)
;; (def-package! ranger
;;   :config
;;   (ranger-override-dired-mode t))
