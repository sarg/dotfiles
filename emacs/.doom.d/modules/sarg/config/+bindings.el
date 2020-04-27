(defun ivy-beginning-or-regex (&rest args)
  (interactive)
  (let ((p (point)))
    (move-beginning-of-line 1)
    (when (= p (point))
      (insert "^"))))

(defun ivy-end-or-regex (&rest args)
  (interactive)
  (let ((p (point)))
    (move-end-of-line 1)
    (when (= p (point))
      (insert "$"))))

(map!
 "C-x b" #'bufler-switch-buffer

 (:map dired-mode-map
   :n (kbd "DEL") #'dired-up-directory)

 (:map ivy-switch-buffer-map
   "C-c C-k" #'ivy-switch-buffer-kill)

 (:map image-mode-map
   :n "q" #'kill-current-buffer)

 (:map dired-mode-map
   :n "K" #'dired-do-kill-lines)
 (:when (featurep! :completion ivy)
   :after ivy
   :map ivy-minibuffer-map
   "C-b" (lambda () (interactive) (insert "WWW "))
   "C-e" #'ivy-end-or-regex
   "C-a" #'ivy-beginning-or-regex))

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key) command)
    (setq key     (pop bindings)
          command (pop bindings))))

(after! exwm
  ;; EXWM
  (spacemacs/exwm-bind-command
   "<XF86AudioRaiseVolume>" #'pulseaudio-control-increase-volume
   "<XF86AudioLowerVolume>" #'pulseaudio-control-decrease-volume
   "<XF86AudioMute>"        #'pulseaudio-control-toggle-current-sink-mute
   "<XF86AudioMicMute>"     #'pulseaudio-control-toggle-current-source-mute

   "<XF86AudioPlay>"    #'emms-pause
   "<XF86AudioNext>"    #'emms-next
   "<XF86AudioPrev>"    #'emms-previous

   "s-f"     #'exwm-layout-toggle-fullscreen
   "<s-tab>" #'exwm-jump-to-last-exwm
   "s-w"     #'exwm-workspace-switch
   "s-r"     #'counsel-linux-app
   "s-c"     #'kill-buffer-and-window

   "s-u"     #'winner-undo
   "S-s-U"   #'winner-redo

   "s-b"     #'bufler-switch-buffer
   "s-g"     #'linkmarks-select

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

   "s-E"     #'sarg/with-browser
   "s-e"    `(lambda () (interactive)
               (sarg/run-or-raise "qutebrowser" "qutebrowser")
               (exwm-workspace-switch (exwm-workspace-name-to-index "brow")))
   "s-s"    `(lambda () (interactive) (sarg/run-or-raise "Slack" "slack"))

   ;; "<s-return>" #'multi-term)
   "<s-return>"   #'+eshell/here
   "<S-s-return>" #'vterm

   "<s-f12>" `(lambda () (interactive) (start-process "flameshot" nil "flameshot" "gui"))
   "<s-delete>" `(lambda () (interactive) (start-process "lock" nil "lock.sh"))))
