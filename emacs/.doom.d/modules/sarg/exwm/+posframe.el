(defun +ivy-posframe-display-exwm (str)
  (ivy-posframe--display str
   (lambda (info)
     (let* ((workarea (elt exwm-workspace--workareas exwm-workspace-current-index))
            (x (aref workarea 0))
            (y (aref workarea 1))

            (fw (aref workarea 2))
            (fh (aref workarea 3))

            (pw (plist-get info :posframe-width))
            (ph (plist-get info :posframe-height)))

       (cons (+ x (/ (- fw pw) 2)) (+ y (/ (- fh ph) 2)))))))

(after! ivy-posframe
  (setq ivy-posframe-display-functions-alist '((t . +ivy-posframe-display-exwm))
        ivy-posframe-parameters '((parent-frame nil)))

  ;; force set frame-position on every posframe display
  (advice-add 'posframe--set-frame-position :before
              (lambda (&rest args)
                (setq-local posframe--last-posframe-pixel-position nil))))
