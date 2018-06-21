;;; private/exwm/config.el -*- lexical-binding: t; -*-

(defcustom pulseaudio-control-default-source "@DEFAULT_SOURCE@"
  "Default Pulse source index to act on."
  :type 'string
  :group 'pulseaudio-control)

(defvar pulseaudio-control--current-source pulseaudio-control-default-source
  "String containing index of currently-selected Pulse sink.")

(defun pulseaudio-control-toggle-current-source-mute ()
  "Toggle muting of currently-selected Pulse sink."
  (interactive)
  (pulseaudio-control--call-pactl (concat "set-source-mute "
                                          pulseaudio-control--current-source
                                          " toggle")))

(defun sarg/exwm-app-launcher ()
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
  (interactive)
  (unless dmenu--cache-executable-files
    (dmenu--cache-executable-files))
  (ivy-read "Run a command: " dmenu--cache-executable-files
            :action (lambda (command) (start-process-shell-command command nil command))
            :caller 'sarg/exwm-app-launcher)
  )

(defun sarg/brightness-change (amount)
  "Adjust screen brightness relatively using the amount given"
  (interactive "NChange screen brightness by: ")
  (let ((brightness-string))
    (if (> 0 amount)
        (setq brightness-string (concat (number-to-string amount) "%"))
      (setq brightness-string (concat "+" (number-to-string amount) "%")))
    (start-process "brightness" nil "xbacklight" brightness-string)))

(defvar sarg-redshift-timer 'nil
  "Stores redshift-adjust timer")

(defun sarg/redshift-adjust ()
  (interactive)
  (start-process-shell-command "redshift" nil "redshift" "-m randr -o"))

(defun sarg/redshift-start ()
  (interactive)
  (unless sarg-redshift-timer
    (setq sarg-redshift-timer (run-at-time nil 60 #'sarg/redshift-adjust))))

(defun sarg/redshift-cancel ()
  (interactive)
  (when sarg-redshift-timer (cancel-timer sarg-redshift-timer))
  (start-process-shell-command "redshift" nil "redshift" "-x"))

(defun sarg/run-or-raise (NAME PROGRAM)
  (interactive)
  (let ((buf (cl-first (ivy--buffer-list NAME))))

    (if buf (switch-to-buffer buf)
      (start-process NAME nil PROGRAM))))

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))


(defun sarg/check-battery ()
  "Checks battery level and makes a warning if it is too low."
  (let* ((status (funcall battery-status-function))
         (charging (string-equal (alist-get ?B status) "Charging"))
         (remain (string-to-number (alist-get ?p status))))

    (if (and (not charging) (< remain 15))
        (alert "Battery too low! Please charge now!"
               :severity 'high))))

(def-package! alert
  :config
  (battery) ; initializes battery-status-function
  (setq alert-default-style 'libnotify)
  (run-at-time nil 60 #'sarg/check-battery))

(def-package! cl-generic :demand)
(def-package! dmenu)
(def-package! pulseaudio-control
  :config
  (setq pulseaudio-control--current-sink "@DEFAULT_SINK@"))

(def-package! xelb)
(def-package! exwm
  :init

  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  :config
  (sarg/redshift-start)

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)
  (setq exwm-workspace-number 5)
  ;; You may want Emacs to show you the time
  (display-time-mode t)
  (setq display-time-24hr-format t)
  ;; (setq-default display-time-format "%H:%M")
  (setq display-time-default-load-average nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)
  (add-hook 'exwm-mode-hook 'evil-emacs-state)
  (add-hook 'exwm-mode-hook 'hide-mode-line-mode)

  (spacemacs/exwm-bind-command
   "<s-f12>" "flameshot gui"
   "<XF86ScreenSaver>" "lock.sh"
   )

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
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; Quick swtiching between workspaces
  (defvar exwm-toggle-workspace 0
    "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")

  (defun exwm-jump-to-last-exwm ()
    (interactive)
    (exwm-workspace-switch exwm-toggle-workspace))

  (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
    (setq exwm-toggle-workspace exwm-workspace-current-index))

  ;; `exwm-input-set-key' allows you to set a global key binding (available in
  ;; any case). Following are a few examples.
  ;; + We always need a way to go back to line-mode from char-mode
  ;; (exwm-input-set-key (kbd "s-c") 'exwm-reset)

  (exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "<s-tab>") #'exwm-jump-to-last-exwm)
  ;; + Bind a key to switch workspace interactively
  (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
  ;; + Set shortcuts to switch to a certain workspace.
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" (+ 1 i)))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch ,i))))

  (exwm-input-set-key (kbd "s-r") #'sarg/exwm-app-launcher)
  (exwm-input-set-key (kbd "s-c") #'kill-buffer-and-window)

  (setq exwm-input-prefix-keys
        '(
          ?\C-x
          ?\M-x
          ?\M-m
          ?\C-g
          ?\C-m
          ?\C-h
          ?\C-р ; cyrillic
          ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map [?\C-c] 'nil)

  ;; Undo window configurations
  (exwm-input-set-key (kbd "s-u") #'winner-undo)
  (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
  ;; Change buffers
  (exwm-input-set-key (kbd "s-b") #'ivy-switch-buffer)
  ;; Focusing windows
  (exwm-input-set-key (kbd "s-h") #'evil-window-left)
  (exwm-input-set-key (kbd "s-j") #'evil-window-down)
  (exwm-input-set-key (kbd "s-k") #'evil-window-up)
  (exwm-input-set-key (kbd "s-l") #'evil-window-right)
  ;; Moving Windows
  (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
  (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
  (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
  (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
  ;; Resize
  (exwm-input-set-key (kbd "M-s-h") #'shrink-window-horizontally)
  (exwm-input-set-key (kbd "M-s-j") #'shrink-window)
  (exwm-input-set-key (kbd "M-s-k") #'enlarge-window)
  (exwm-input-set-key (kbd "M-s-l") #'enlarge-window-horizontally)

  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'pulseaudio-control-toggle-current-source-mute)

  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")
                      `(lambda ()
                         (interactive)
                         (sarg/brightness-change 10)
                         ))

  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>")
                      `(lambda ()
                         (interactive)
                         (sarg/brightness-change -10)
                         ))

  ;; (exwm-input-set-key (kbd "s-i")
  ;;                     `(lambda ()
  ;;                        (interactive)
  ;;                        (sarg/run-or-raise "TelegramDesktop" "telegram-desktop")))


  (exwm-input-set-key (kbd "<s-return>")
                      `(lambda ()
                         (interactive)
                         (start-process "terminal" nil "my-terminal")))

  (exwm-input-set-key (kbd "s-e")
                      `(lambda ()
                         (interactive)
                         (sarg/run-or-raise "qutebrowser" "qutebrowser")))

  (exwm-input-set-key (kbd "s-E") #'sarg/with-browser)

  ;; (exwm-input-set-key (kbd "C-\\")
  ;;                     `(lambda ()
  ;;                        (interactive)
  ;;                        (start-process "xkb-switch" nil "xkb-switch" "-n")))

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t)

  (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --auto --output LVDS1 --off --output DP2 --mode 2560x1440")))
  (exwm-randr-enable)
  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
  ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
  ;; sequence (of type vector or string), while DEST can also be a single key.

  (exwm-input-set-simulation-keys
   (mapcar (lambda (c) (cons (kbd (car c)) (cdr c)))
           '(
             ;; ("C-b" . left)
             ;; ("C-f" . right)
             ;; ("C-p" . up)
             ("C-m" . return)
             ;; ("C-n" . down)
             ("DEL" . backspace)
             ("C-р" . backspace)
             )))

  (exwm-input-set-key (kbd "s-.") (lambda () (interactive) (message (format-time-string "%Y-%m-%d %T (%a w%W)"))))

  (setq exwm-manage-configurations '(
                                     ((equal exwm-class-name "Peek")
                                      floating t
                                      floating-mode-line nil
                                      )
                                     ((equal exwm-class-name "TelegramDesktop")
                                      floating t
                                      floating-mode-line nil
                                      x 0.73
                                      y 0.02
                                      width 0.25
                                      height 0.8
                                      )))

  ;; (exwm-input-set-simulation-keys nil)

  ;; Do not forget to enable EXWM. It will start by itself when things are ready.
  ;; (exwm-enable)
  )


(defun sarg/with-browser ()
  "Opens browser side-by-side with current window"
  (interactive)
  (delete-other-windows)
  (set-window-buffer (split-window-horizontally) "qutebrowser"))

(use-package fate
  :load-path "~/devel/ext/fate"
  :config
  (setq fate:data-file "~/.events/win")
  (defun fate:state-string-base (left right)
    "Represent state using LEFT and RIGHT."
    (format "%s;win;%s;%s\n"
            (format-time-string "%s.%6N")
            (fate:escape left)
            (fate:escape right))))
