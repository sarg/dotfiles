;;; app/exwm/config.el -*- lexical-binding: t; -*-
(defun exwm/bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (if (vectorp key) key (kbd key))
                        (if (functionp command) command
                          `(lambda () (interactive)
                             (start-process-shell-command "exwm-bind" nil ,command))))
    (setq key (pop bindings)
          command (pop bindings)))

  (exwm-input--update-global-prefix-keys))

(defun exwm/switch-to-next-buffer (class &optional reverse?)
  "Form a cycle of CLASS buffers and return the one next to current buffer.
REVERSE? it when true. Returns the selected buffer."
  (cl-loop
   with first with this with next
   for b in (if reverse? (reverse (buffer-list)) (buffer-list))
   when (string= class (buffer-local-value 'exwm-class-name b)) do
   (setf first (or first b))
   (setf next (and this b))
   (setf this (or this (when (eq (current-buffer) b) b)))
   until next
   finally return
   (and (or next first)
        (switch-to-buffer (or next first) t t))))

(defun exwm/clean-killed-buffers ()
  "Remove killed buffers from exwm--id-buffer-alist."
  (interactive)
  (setq exwm--id-buffer-alist
        (seq-filter (lambda (e) (buffer-live-p (cdr e))) exwm--id-buffer-alist)))

(defun exwm/init ()
  (when (modulep! +lemonbar)
    (exwm/lemonbar-mode))

  (when (modulep! +polybar)
    (exwm/polybar-mode))

  (call-process "pkill" nil nil nil "-x" "-USR2" "xsecurelock"))

(defun exwm/update-title-hook ()
  (exwm-workspace-rename-buffer exwm-title))

(use-package! exwm
  :commands (exwm-enable)
  :demand
  :hook (exwm-init . exwm/init)
  :hook (exwm-update-title . exwm/update-title-hook)

  :init
  (set-popup-rule! "^\\*EXWM\\*$" :ignore t)

  :config
  (add-to-list 'doom-real-buffer-modes 'exwm-mode)
  (when (modulep! +workspaces) (load! "+workspaces"))
  (load! "+patch")
  (load! "+xkb-layout-switch")

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  (add-to-list 'evil-emacs-state-modes 'exwm-mode)

  (def-modeline! 'exwm `(" %b") `())
  (set-modeline-hook! 'exwm-mode-hook 'exwm)

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t))

;; Minor mode to disable the screensaver when one or more X clients are fullscreen. ;;
(use-package! exwm-ss
  :after exwm
  :config
  (defclass exwm-ss--xset (exwm-ss)
    ()
    :documentation "EXWM-SS implementation which uses xset(1) to disable screensaver.")

  (cl-defmethod exwm-ss-set-inhibited! :after ((ss exwm-ss--xset) inhibit)
    (let ((cmd (format "xset s %s" (if inhibit "off" "300 5"))))
      (exwm-ss--with-home (start-process-shell-command "exwm-ss-xset" nil cmd))))

  (defclass exwm-ss-control-xset (exwm-ss-control-base exwm-ss--dpms-command exwm-ss--xset)
    ()
    :documentation "xset-based `EXWM-SS-CONTROL' implementation.")

  (setq exwm-ss-controller-class 'exwm-ss-control-xset)
  (exwm-ss-mode 1))
