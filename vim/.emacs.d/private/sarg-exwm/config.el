(with-eval-after-load 'exwm
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'pulseaudio-control-increase-volume)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'pulseaudio-control-decrease-volume)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'pulseaudio-control-toggle-current-sink-mute)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'pulseaudio-control-toggle-current-source-mute)

  ;; (require 'exwm)
  ;; (require 'exwm-config)

  ;; (setq exwm-workspace-number 4)
  ;; ;; Make class name the buffer name
  ;; (add-hook 'exwm-update-class-hook
  ;;           (lambda ()
  ;;             (exwm-workspace-rename-buffer exwm-class-name)))
  ;; ;; 's-r': Reset
  ;; (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; ;; 's-w': Switch workspace
  ;; (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; ;; 's-N': Switch to certain workspace
  ;; (dotimes (i 10)
  ;;   (exwm-input-set-key (kbd (format "s-%d" i))
  ;;                       `(lambda ()
  ;;                          (interactive)
  ;;                          (exwm-workspace-switch-create ,i))))
  ;; ;; 's-&': Launch application
  ;; (exwm-input-set-key (kbd "s-&")
  ;;                     (lambda (command)
  ;;                       (interactive (list (read-shell-command "$ ")))
  ;;                       (start-process-shell-command command nil command)))
  ;; ;; Line-editing shortcuts
  ;; (exwm-input-set-simulation-keys
  ;;  '(([?\C-b] . left)
  ;;    ([?\C-f] . right)
  ;;    ([?\C-p] . up)
  ;;    ([?\C-n] . down)
  ;;    ([?\C-a] . home)
  ;;    ([?\C-e] . end)

  ;;    ([?\M-v] . prior)
  ;;    ([?\C-v] . next)
  ;;    ([?\C-d] . delete)
  ;;    ([?\C-k] . (S-end delete))))
  ;; ;; Configure Ido
  ;; (exwm-config-ido)
  ;; ;; Other configurations
  ;; (exwm-config-misc)

  ;; (setq exwm-layout-show-all-buffers t
  ;;       exwm-workspace-show-all-buffers t
  ;;       )

  ;; (exwm-input-set-simulation-keys
  ;;  '(([?\C-b] . left)
  ;;    ([?\C-f] . right)
  ;;    ([?\C-p] . up)
  ;;    ([?\C-n] . down)
  ;;    ([?\C-a] . home)
  ;;    ([?\C-e] . end)
  ;;    ([?\M-v] . prior)
  ;;    ([?\C-v] . next)
  ;;    ([?\C-d] . delete)
  ;;    ([?\C-k] . (S-end delete))))
  )
