(require 'exwm-randr)

(defvar xrandr-executable (executable-find "xrandr"))
(defun exwm-change-screen-hook ()
  (interactive)
  (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process xrandr-executable nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
          (call-process xrandr-executable nil nil nil "--auto")
        (call-process
         xrandr-executable nil nil nil
         "--output" (match-string 1) "--primary" "--auto"
         "--output" default-output "--below" (match-string 1))
        (setq exwm-randr-workspace-output-plist
              (list 0 default-output
                    1 (match-string 1)))))))


(add-hook 'exwm-randr-screen-change-hook #'exwm-change-screen-hook)
(exwm-randr-enable)
