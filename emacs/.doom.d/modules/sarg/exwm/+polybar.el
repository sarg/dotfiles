(defvar sarg/polybar-process nil)

(define-minor-mode sarg/polybar-mode
  "Show polybar."
  :init-value nil
  :global t

  (if sarg/polybar-mode
      (progn
        (exwm-workspace--update-ewmh-desktop-names)
        (setq sarg/polybar-process
              (start-process "polybar" nil
                             "polybar" "panel")))

    (when sarg/polybar-process
      (interrupt-process sarg/polybar-process)
      (setq sarg/polybar-process nil))))

(defun exwm-workspace--update-ewmh-desktop-names ()
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_NAMES
                     :window exwm--root :data
                     (mapconcat (lambda (i) (funcall exwm-workspace-index-map i))
                                (number-sequence 0 (1- (exwm-workspace--count)))
                                "\0"))))

(add-hook 'exwm-workspace-list-change-hook
          #'exwm-workspace--update-ewmh-desktop-names)
(add-hook 'exwm-init-hook
          #'sarg/polybar-mode)
