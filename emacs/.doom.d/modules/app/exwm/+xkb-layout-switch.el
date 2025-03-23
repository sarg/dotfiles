(defvar-local exwm-input-current-group 0)
(defvar exwm-input--numGroups 2)
(defvar exwm/input-layout-names nil)

(defun exwm-input--on-xkb-StateNotify (data _synthetic)
  "Handle XKB State change."
  (exwm--log)
  (let ((obj1 (make-instance 'xcb:xkb:StateNotify)))
    (xcb:unmarshal obj1 data)
    (with-slots (group changed) obj1
      (when (/= 0 (logand changed xcb:xkb:StatePart:GroupState))
        (setq exwm-input-current-group group)))))

(defun exwm-input--layout-names ()
  (interactive)
  (mapcar #'x-get-atom-name

          (slot-value
           (xcb:+request-unchecked+reply exwm--connection
               (make-instance
                'xcb:xkb:GetNames
                :deviceSpec xcb:xkb:ID:UseCoreKbd
                :which xcb:xkb:NameDetail:GroupNames))
           'groups)))

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
   (mod (+ (or inc 1) exwm-input-current-group)
        exwm-input--numGroups)))

(defun exwm-xkb-reset-layout ()
  (interactive)
  (exwm-xkb-set-layout 0))

(defun exwm-xkb-layout-init ()
  (setq exwm-input--numGroups
        (slot-value
         (xcb:+request-unchecked+reply exwm--connection
             (make-instance
              'xcb:xkb:GetControls
              :deviceSpec xcb:xkb:ID:UseCoreKbd))
         'numGroups)

        exwm/input-layout-names
        (exwm-input--layout-names))

  (xcb:+event exwm--connection 'xcb:xkb:StateNotify
              #'exwm-input--on-xkb-StateNotify)

  (xcb:+request exwm--connection
      (make-instance 'xcb:xkb:SelectEvents
                     :deviceSpec xcb:xkb:ID:UseCoreKbd
                     :affectWhich (logior xcb:xkb:EventType:StateNotify)
                     :clear 0
                     :selectAll 0
                     :affectMap 0
                     :map 0
                     :affectState (logior xcb:xkb:StatePart:GroupState)
                     :stateDetails (logior xcb:xkb:StatePart:GroupState)))

  (xcb:flush exwm--connection)

  (add-hook! doom-switch-window #'exwm-xkb-reset-layout)
  (add-hook! exwm-exit (remove-hook! doom-switch-window #'exwm-xkb-reset-layout)))

(add-hook! exwm-init #'exwm-xkb-layout-init)
