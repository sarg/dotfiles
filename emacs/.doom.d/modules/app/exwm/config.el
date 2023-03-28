;;; private/exwm/config.el -*- lexical-binding: t; -*-
(defun exwm-set-default-cursor ()
  "Sets default cursor to left_ptr (instead of default black cross)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:Cursor
                     :cursor (xcb:cursor:load-cursor exwm--connection "left_ptr"))))

(defun doom/exwm-new-window-hook ()
  (doom-mark-buffer-as-real-h)
  ;; no modelines please
  (+modeline-mode -1)
  (hide-mode-line-mode +1))

(use-package! exwm
  :commands (exwm-enable exwm-init)
  :hook (exwm-init . exwm-set-default-cursor)
  :hook (exwm-mode . doom/exwm-new-window-hook)

  :init
  (setq exwm-leader-key "<f13>")
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  :config
  (load! "+workspaces")
  (load! "+polybar")
  (load! "+bindings")
  (load! "+redshift")
  (load! "+xkb-layout-switch")

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  ;; when a new window class name or title is available. Here's some advice on
  ;; this subject:
  ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
  ;; + Only renaming buffer in one hook and avoid it in the other. There's no
  ;;   guarantee on the order in which they are run.
  ;; + For applications with multiple windows (e.g. GIMP), the class names of all
  ;;   windows are probably the same. Using window titles for them makes more
  ;;   sense.
  ;; + Some application change its title frequently (e.g. browser, terminal).
  ;;   Its class name may be more suitable for such case.
  ;; In the following example, we use class names for all windows expect for
  ;; Java applications and GIMP.
  ;;
  (defun meaningful-title? ()
    (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
        (string= "qutebrowser" exwm-class-name)
        (string= "OpenSCAD" exwm-class-name)
        (string= "gimp" exwm-instance-name)))

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (meaningful-title?)
                (exwm-workspace-rename-buffer exwm-class-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name) (meaningful-title?))
                (exwm-workspace-rename-buffer exwm-title))))

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
