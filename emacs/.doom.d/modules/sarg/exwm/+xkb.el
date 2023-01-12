(defvar exwm-input--numGroups 2)

(defun exwm-input--current-group ()
  (interactive)
  (slot-value
   (xcb:+request-unchecked+reply exwm--connection
       (make-instance
        'xcb:xkb:GetState
        :deviceSpec xcb:xkb:ID:UseCoreKbd))
   'group))

(defun exwm-xkb-set-layout (num)
  (interactive)
  (xcb:+request-checked exwm--connection
      (make-instance
       'xcb:xkb:LatchLockState
       :deviceSpec xcb:xkb:ID:UseCoreKbd
       :affectModLocks 0
       :modLocks 0
       :lockGroup 1
       :groupLock num
       :affectModLatches 0
       :latchGroup 0
       :groupLatch 0))
  (xcb:flush exwm--connection))

(defun exwm-xkb-next-layout (&optional inc)
  (interactive)
  (exwm-xkb-set-layout
   (mod (+ (or inc 1) (exwm-input--current-group))
        exwm-input--numGroups)))

(define-key exwm-mode-map
  (kbd "C-\\") #'exwm-xkb-next-layout)

(defun exwm-xkb-reset-layout ()
  (interactive)
  (exwm-xkb-set-layout 0))

(add-hook! exwm-init
  (setq exwm-input--numGroups
        (slot-value
         (xcb:+request-unchecked+reply exwm--connection
             (make-instance
              'xcb:xkb:GetControls
              :deviceSpec xcb:xkb:ID:UseCoreKbd))
         'numGroups))

  (add-hook! doom-switch-window #'exwm-xkb-reset-layout)
  (add-hook! exwm-exit (remove-hook! doom-switch-window #'exwm-xkb-reset-layout)))

;; (require 'exwm-xim)
;; (exwm-xim-enable)
