(defun exwm--update-hints (id &optional force)
  "Update hints from WM_HINTS.
Argument ID contains the X window of the `exwm-mode' buffer.

When FORCE is nil the update only takes place if both of
`exwm--hints-input' and `exwm--hints-urgency' are unset."
  (exwm--log "#x%x" id)
  (with-current-buffer (exwm--id->buffer id)
    (unless (and (not force) exwm--hints-input exwm--hints-urgency)
      (let ((reply (xcb:+request-unchecked+reply exwm--connection
                       (make-instance 'xcb:icccm:get-WM_HINTS :window id))))
        (when (and reply (slot-value reply 'flags)) ;nil when destroyed
          (with-slots (flags input initial-state) reply
            (when flags
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:InputHint))
                (setq exwm--hints-input (when input (= 1 input))))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:StateHint))
                (when (= xcb:icccm:WM_STATE:WithdrawnState exwm-state)
                  (setq exwm-state initial-state)))
              (unless (= 0 (logand flags xcb:icccm:WM_HINTS:UrgencyHint))
                (setq exwm--hints-urgency t))))
          (when (and exwm--hints-urgency
                     (not (eq exwm--frame exwm-workspace--current)))
            (unless (frame-parameter exwm--frame 'exwm-urgency)
              (set-frame-parameter exwm--frame 'exwm-urgency t)
              (setq exwm-workspace--switch-history-outdated t))))))))
