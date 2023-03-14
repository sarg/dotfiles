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

(use-package! bluetooth)
(use-package! exwm
  :commands (exwm-enable exwm-init)
  :hook (exwm-init . exwm-set-default-cursor)
  :hook (exwm-mode . doom/exwm-new-window-hook)

  :init
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  :config
  (load! "+udisks")
  (load! "+workspaces")
  (load! "+polybar")
  (load! "+bindings")
  (load! "+volume")
  (load! "+redshift")
  (load! "+brightness")
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

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\M-x
          ?\M-m
          ?\C-g
          ?\C-m
          ?\C-h
          ?\C-р                         ; cyrillic
          ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map [?\C-c] 'nil)

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t)

  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
  ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
  ;; sequence (of type vector or string), while DEST can also be a single key.

  (setq exwm-input-simulation-keys
        (mapcar (lambda (c) (cons (kbd (car c)) (cdr c)))
                `(
                  ;; ("C-b" . left)
                  ;; ("C-f" . right)
                  ;; ("C-p" . up)
                  ("C-m" . return)
                  ;; ("C-n" . down)
                  ("DEL" . backspace)
                  ("C-р" . backspace))))


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

(use-package! fate
  :disabled
  :load-path "~/devel/ext/fate"
  :config
  (setq fate:data-file "~/.events/win")

  (defun fate:buffer-string (buffer)
    "Return either path or name of BUFFER."
    (or (buffer-file-name buffer)
        (buffer-name buffer)))

  (defun fate:log-state (state)
    "Write STATE to the database file."
    (write-region state nil fate:data-file 'append :inhibit))

  (defun fate:state-string-base (left right)
    "Represent state using LEFT and RIGHT."
    (format "%s;win;%s;%s\n"
            (format-time-string "%s.%6N")
            (fate:escape left)
            (fate:escape right))))
