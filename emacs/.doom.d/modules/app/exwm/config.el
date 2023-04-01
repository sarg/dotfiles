;;; private/exwm/config.el -*- lexical-binding: t; -*-
(defun exwm-set-default-cursor ()
  "Sets default cursor to left_ptr (instead of default black cross)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:Cursor
                     :cursor (xcb:cursor:load-cursor exwm--connection "left_ptr"))))

(defun exwm/init ()
  (load! "+bar")
  (exwm/bar-mode)
  (exwm-set-default-cursor))

(defun doom/exwm-new-window-hook ()
  ;; no modelines please
  ;; (+modeline-mode -1)
  ;; (hide-mode-line-mode +1)
  (doom-mark-buffer-as-real-h))

(use-package! exwm
  :commands (exwm-enable exwm-init)
  :hook (exwm-init . exwm/init)
  :hook (exwm-mode . doom/exwm-new-window-hook)

  :init
  (setq exwm-leader-key "<f13>")
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  :config
  (load! "+workspaces")
  (load! "+bindings")
  (load! "+redshift")
  (load! "+xkb-layout-switch")

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)

  (add-hook 'exwm-update-title-hook
            (lambda () (exwm-workspace-rename-buffer exwm-title)))

  (def-modeline! 'exwm
                 `("" (:eval (buffer-name)))
                 `())

  (set-modeline-hook! 'exwm-mode-hook 'exwm)

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t)

  (setq exwm-manage-configurations
        `(((-any? (lambda (el) (equal exwm-class-name el))
            '("Peek" "mpv" "scrcpy" "AusweisApp2"))
           floating t
           floating-mode-line nil)
          ((equal exwm-instance-name "sun-awt-X11-XDialogPeer")
           managed t
           floating t)
          ((equal exwm-class-name "qutebrowser")
           workspace ,(exwm-workspace-name-to-index "brow")))))

;; Minor mode to disable the screensaver when one or more X clients are fullscreen. ;;
(use-package! exwm-ss
  :defer t
  :after exwm
  :init (exwm-ss-mode 1))

(use-package! exwm-edit
  :disabled
  :custom
  (exwm-edit-bind-default-keys nil)

  :config
  (defalias 'exwm-edit--display-buffer 'pop-to-buffer)

  :init
  (set-popup-rule! "^\\*exwm-edit"
    :side 'bottom :size 0.2
    :select t :quit nil :ttl t))
