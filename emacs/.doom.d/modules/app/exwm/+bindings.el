(defun exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key) command)
    (setq key     (pop bindings)
          command (pop bindings))))

(defun sarg/run-or-raise (NAME PROGRAM &rest ARGS)
  (interactive)
  (let ((buf (cl-find-if
              (lambda (buf) (string= NAME (buffer-name buf)))
              (buffer-list))))

    (if buf (switch-to-buffer buf)
      (apply #'start-process
             (append (list NAME nil "setsid" "-w" PROGRAM) ARGS)))))

(exwm-bind-command
   "<XF86AudioPlay>"    #'emms-pause
   "<XF86AudioNext>"    #'emms-next
   "<XF86AudioPrev>"    #'emms-previous

   "s-f"     #'exwm-layout-toggle-fullscreen
   "s-r"     #'app-launcher-run-app
   "s-c"     #'kill-buffer-and-window

   "s-u"     #'winner-undo
   "S-s-U"   #'winner-redo

   "s-h"     #'evil-window-left
   "s-j"     #'evil-window-down
   "s-k"     #'evil-window-up
   "s-l"     #'evil-window-right

   "s-H"     #'evil-window-move-far-left
   "s-J"     #'evil-window-move-very-bottom
   "s-K"     #'evil-window-move-very-top
   "s-L"     #'evil-window-move-far-right

   "M-s-h"   #'shrink-window-horizontally
   "M-s-j"   #'shrink-window
   "M-s-k"   #'enlarge-window
   "M-s-l"   #'enlarge-window-horizontally

   "s-e"    `(lambda () (interactive)
               (sarg/run-or-raise "qutebrowser" "qutebrowser" "--qt-arg" "no-sandbox" "true")
               (exwm-workspace-switch (exwm-workspace-name-to-index "brow")))

   "<s-return>"   #'+eshell/here
   "<S-s-return>" #'+vterm/here

   "<s-f12>" `(lambda () (interactive) (start-process "flameshot" nil "flameshot" "gui"))
   "<s-delete>" `(lambda () (interactive) (start-process "lock" nil "lock.sh")))
