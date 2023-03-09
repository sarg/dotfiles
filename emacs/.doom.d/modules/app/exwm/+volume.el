(use-package! pulseaudio-control
  :custom
  (pulseaudio-control-use-default-sink t)
  (pulseaudio-control-use-default-source t)
  (pulseaudio-control-volume-step "5%")

  :config
  (exwm-bind-command
   "<XF86AudioRaiseVolume>" #'pulseaudio-control-increase-sink-volume
   "<XF86AudioLowerVolume>" #'pulseaudio-control-decrease-sink-volume
   "<XF86AudioMute>"        #'pulseaudio-control-toggle-current-sink-mute
   "<XF86AudioMicMute>"     #'pulseaudio-control-toggle-current-source-mute))
