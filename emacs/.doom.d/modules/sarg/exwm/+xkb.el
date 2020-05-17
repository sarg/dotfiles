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

(advice-add
 'toggle-input-method
 :before-until
 (lambda (&rest args) (interactive)
   (when (eq major-mode 'exwm-mode)
     (exwm-xkb-next-layout) t)))

(add-to-list 'exwm-input-prefix-keys ?\C-\\)

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

  (add-to-list 'doom-switch-window-hook #'exwm-xkb-reset-layout))

;; (use-package reverse-im
;;   :ensure t
;;   :custom
;;   (reverse-im-input-methods '("russian-computer"))
;;   :config
;;   (reverse-im-mode t))

;; (setq reverse-im-input-methods '("russian-computer"))

(require 'exwm-xim)
(exwm-xim-enable)
