;;; private/sarg/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+elfeed")
(load! "+email")
(load! "+org")

(setq auth-sources '(password-store))
(after! magit
    (setq magit-process-find-password-functions '(magit-process-password-auth-source)))

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
(def-package! nginx-mode)
(def-package! slack
  :init
  (set-popup-rule! "^\\*Slack" :ignore t)
  (setq slack-prefer-current-team t)

  :commands (slack-start)

  :config
  (load! "+slack"))

(def-package! webpaste)
;; (def-package! ranger
;;   :config
;;   (ranger-override-dired-mode t))
