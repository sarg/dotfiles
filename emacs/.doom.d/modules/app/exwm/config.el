;;; app/exwm/config.el -*- lexical-binding: t; -*-
(defun exwm-set-default-cursor ()
  "Sets default cursor to left_ptr (instead of default black cross)."
  (xcb:+request exwm--connection
      (make-instance 'xcb:ChangeWindowAttributes
                     :window exwm--root
                     :value-mask xcb:CW:Cursor
                     :cursor (xcb:cursor:load-cursor exwm--connection "left_ptr"))))

(defun exwm/bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (if (vectorp key) key (kbd key))
                        (if (functionp command) command
                          `(lambda () (interactive)
                            (start-process-shell-command "exwm-bind" nil ,command))))
    (setq key (pop bindings)
          command (pop bindings))))

(defun exwm/init ()
  (when (modulep! +bar)
    (exwm/bar-mode))

  (when (modulep! +polybar)
    (exwm/polybar-mode))
  (exwm-set-default-cursor))

(defun exwm/new-window-hook ()
  (doom-mark-buffer-as-real-h))

(defun exwm/update-title-hook ()
  (exwm-workspace-rename-buffer exwm-title))

(use-package! exwm
  :commands (exwm-enable)
  :hook (exwm-init . exwm/init)
  :hook (exwm-mode . exwm/new-window-hook)
  :hook (exwm-update-title . exwm/update-title-hook)

  :init
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  :config
  (load! "+workspaces")
  (load! "+xkb-layout-switch")

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)

  (def-modeline! 'exwm `("%b") `())
  (set-modeline-hook! 'exwm-mode-hook 'exwm)

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t))

;; Minor mode to disable the screensaver when one or more X clients are fullscreen. ;;
(use-package! exwm-ss
  :init (exwm-ss-mode 1))
