(defvar exwm-workspace-names '("code" "brow" "extr" "lisp" ))
(setq exwm-workspace-number (length exwm-workspace-names))
(defsubst exwm-workspace-name-to-index (name)
  (-elem-index name exwm-workspace-names))

(setq exwm-workspace-index-map
      (lambda (index)
        (if (< index (length exwm-workspace-names))
            (elt exwm-workspace-names index)
          (number-to-string index))))

;; Quick switching between workspaces
(defvar exwm-toggle-workspace 0
  "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")

(defun exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(defadvice exwm-workspace-switch (before save-toggle-workspace activate)
  (setq exwm-toggle-workspace exwm-workspace-current-index))

;; + Set shortcuts to switch to a certain workspace.
;; use all digits, so that if new workspace created it could be switched to
(dotimes (i 8)
  (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch ,i))))

;; EWMH integration
(defun exwm-workspace--update-ewmh-desktop-names ()
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_NAMES
                     :window exwm--root :data
                     (mapconcat (lambda (i) (funcall exwm-workspace-index-map i))
                                (number-sequence 0 (1- (exwm-workspace--count)))
                                "\0"))))

(add-hook 'exwm-workspace-list-change-hook
          #'exwm-workspace--update-ewmh-desktop-names)
