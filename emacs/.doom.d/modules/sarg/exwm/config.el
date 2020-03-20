;;; private/exwm/config.el -*- lexical-binding: t; -*-

(defun exwm-change-screen-hook ()
  (interactive)
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1) "--primary" "--auto"
         "--output" default-output "--below" (match-string 1))
        (setq exwm-randr-workspace-output-plist
              (list 0 default-output
                    1 (match-string 1)))))))

(load! "+posframe")

(defun sarg/exwm-app-launcher ()
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
  (interactive)
  (unless dmenu--cache-executable-files
    (dmenu--cache-executable-files))
  (ivy-read "Run a command: " dmenu--cache-executable-files
            :action (lambda (command) (start-process-shell-command command nil command))
            :caller 'sarg/exwm-app-launcher))

(defun sarg/run-or-raise (NAME PROGRAM)
  (interactive)
  (let ((buf (cl-find-if
              (lambda (buf) (string= NAME (buffer-name buf)))
              (buffer-list))))

    (if buf (switch-to-buffer buf)
      (start-process NAME nil PROGRAM))))

(defun sarg/shell-cmd (command)
  `(lambda ()
     (interactive)
     (start-process-shell-command ,command nil ,command)))


(battery) ; initializes battery-status-function
(run-at-time nil 60 #'sarg/check-battery)
(defun sarg/check-battery ()
  "Checks battery level and makes a warning if it is too low."
  (let* ((status (funcall battery-status-function))
         (charging (string-equal (alist-get ?B status) "Charging"))
         (remain (string-to-number (alist-get ?p status))))

    (if (and (not charging) (< remain 15))
        (notifications-notify :body "Battery too low! Please charge now!" :urgency 'critical))))

;; props to https://github.com/ch11ng/exwm/issues/593
(defun my-exwm-workspace-switch-to-buffer (orig-func buffer-or-name &rest args)
  (when buffer-or-name
    (if (and (get-buffer buffer-or-name)
             (or exwm--floating-frame
                 (with-current-buffer buffer-or-name exwm--floating-frame)))
        (exwm-workspace-switch-to-buffer buffer-or-name)
      (apply orig-func buffer-or-name args))))

(use-package! dmenu)
;; (use-package! gpastel)
;; (use-package! exwm-mff)
(use-package! xelb)
(use-package! exwm
  :commands (exwm-enable exwm-init)
  ;; :hook (exwm-init . exwm-mff-mode)
  :hook (exwm-mode . doom-mark-buffer-as-real-h)

  :init
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)
  (load! "+polybar")

  ;; (add-hook! 'exwm-init-hook 'gpastel-start-listening)

  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)
  ;; (setq mouse-autoselect-window t
  ;;       focus-follows-mouse t)

  :config
  (load! "+brightness")

  (advice-add 'switch-to-buffer :around 'my-exwm-workspace-switch-to-buffer)
  (advice-add 'ivy--switch-buffer-action :around 'my-exwm-workspace-switch-to-buffer)

  (exwm-input-set-key (kbd "M-y") #'my/exwm-counsel-yank-pop)

  (defun my/exwm-counsel-yank-pop ()
    "Same as `counsel-yank-pop' and paste into exwm buffer."
    (interactive)
    (let ((inhibit-read-only t)
          ;; Make sure we send selected yank-pop candidate to
          ;; clipboard:
          (yank-pop-change-selection t))
      (call-interactively #'counsel-yank-pop))
    (when (derived-mode-p 'exwm-mode)
      ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (exwm-input--fake-key ?\C-v)))

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)
  (setq exwm-workspace-number 5)
  (defvar exwm-workspace-names
    '("code" "brow" "extr" "slac" "lisp" ))

  (defsubst exwm-workspace-name-to-index (name)
    (-elem-index name exwm-workspace-names))

  (setq exwm-workspace-index-map
        (lambda (index)
          (if (< index (length exwm-workspace-names))
              (elt exwm-workspace-names index)
            (number-to-string index))))
 
  ;; You may want Emacs to show you the time
  ;; (display-time-mode t)
  ;; (setq display-time-24hr-format t)
  ;; (setq-default display-time-format "%H:%M")
  ;; (setq display-time-default-load-average nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)
  (add-hook 'exwm-mode-hook 'evil-emacs-state)
  (add-hook 'exwm-mode-hook 'hide-mode-line-mode)

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
                          (string= "qutebrowser" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))

  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string= "qutebrowser" exwm-instance-name)
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

  ;; + Set shortcuts to switch to a certain workspace.
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch ,i))))



  (when (featurep! :app telega +ivy)
    (exwm-input-set-key (kbd "s-i") #'sarg/sauron-show))

  (setq exwm-input-prefix-keys
        '(?\C-\\                        ; xim
          ?\C-x
          ?\M-x
          ?\M-m
          ?\C-g
          ?\C-m
          ?\C-h
          ?\C-р                         ; cyrillic
          ))

  (require 'exwm-xim)
  (exwm-xim-enable)

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (define-key exwm-mode-map [?\C-c] 'nil)

  ;; Undo window configurations


  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t)

  (require 'exwm-randr)
  ;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (add-hook 'exwm-randr-screen-change-hook #'exwm-change-screen-hook)
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

  (exwm-input-set-key (kbd "s-.") (lambda () (interactive) (message "%s %s"
                                                               (concat (format-time-string "%Y-%m-%d %T (%a w%W)"))
                                                               (battery-format "| %L: %p%% (%t)" (funcall battery-status-function)))))

  (setq exwm-manage-configurations
        `(((-any? (lambda (el) (equal exwm-class-name el))
                  '("Peek" "mpv" "scrcpy"))
           floating t
           floating-mode-line nil)
          ((equal exwm-class-name "Slack")
           workspace ,(exwm-workspace-name-to-index "slac"))
          ((equal exwm-instance-name "sun-awt-X11-XDialogPeer")
           managed t
           floating t)
          ((equal exwm-class-name "jetbrains-idea")
           workspace ,(exwm-workspace-name-to-index "code"))
          ((equal exwm-class-name "qutebrowser")
           workspace ,(exwm-workspace-name-to-index "brow"))
          ((equal exwm-class-name "TelegramDesktop")
           floating t
           floating-mode-line nil
           x 0.73
           y 0.02
           width 0.25
           height 0.8)))

  ;; (exwm-input-set-simulation-keys nil)

  ;; Do not forget to enable EXWM. It will start by itself when things are ready.
  ;; (exwm-enable)
  )


(defun sarg/with-browser ()
  "Opens browser side-by-side with current window"
  (interactive)
  (delete-other-windows)
  (set-window-buffer (split-window-horizontally) "qutebrowser"))

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
