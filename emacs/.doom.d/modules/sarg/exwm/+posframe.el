(defun sarg/ivy-posframe-poshandler (info)
  (let ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
        (return-value (posframe-poshandler-frame-center info)))

    (cons (+ (aref workarea 0) (car return-value))
          (+ (aref workarea 1) (cdr return-value)))))

(defun ivy-posframe-display-at-exwm (str)
  (ivy-posframe--display str #'sarg/ivy-posframe-poshandler))

(after! ivy-posframe
  (setq ivy-posframe-border-width 4
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
        ivy-posframe-style 'exwm
        ivy-posframe-parameters '((parent-frame nil))))
