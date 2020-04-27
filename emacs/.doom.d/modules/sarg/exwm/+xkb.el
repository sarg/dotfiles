(defvar exwm-input--numGroups
  (slot-value
   (xcb:+request-unchecked+reply exwm--connection
       (make-instance
        'xcb:xkb:GetControls
        :deviceSpec xcb:xkb:ID:UseCoreKbd))
   'numGroups))

(defun exwm-input--current-group ()
  (interactive)
  (slot-value
   (xcb:+request-unchecked+reply exwm--connection
       (make-instance
        'xcb:xkb:GetState
        :deviceSpec xcb:xkb:ID:UseCoreKbd))
   'group))

(defun exwm-xkb-next-layout (&optional inc)
  (interactive)
  (xcb:+request-checked exwm--connection
      (make-instance
       'xcb:xkb:LatchLockState
       :deviceSpec xcb:xkb:ID:UseCoreKbd
       :affectModLocks 0
       :modLocks 0
       :lockGroup 1
       :groupLock (mod (+ (or inc 1) (exwm-input--current-group))
                       exwm-input--numGroups)
       :affectModLatches 0
       :latchGroup 0
       :groupLatch 0))
  (xcb:flush exwm--connection))

(advice-add
 'toggle-input-method
 :before-until
 (lambda () (interactive)
   (when (eq major-mode 'exwm-mode)
     (exwm-xkb-next-layout) t)))

(add-to-list 'exwm-input-prefix-keys ?\C-\\)

(require 'exwm-xim)
(exwm-xim-enable)
