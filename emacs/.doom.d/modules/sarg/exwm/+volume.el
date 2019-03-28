(def-package! pulseaudio-control
  :load-path "~/devel/ext/pulseaudio-control/"

  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "5%"
        pulseaudio-control--current-source "@DEFAULT_SOURCE@")

  (spacemacs/exwm-bind-command
   "<XF86AudioRaiseVolume>" #'pulseaudio-control-increase-volume
   "<XF86AudioLowerVolume>" #'pulseaudio-control-decrease-volume
   "<XF86AudioMute>"        #'pulseaudio-control-toggle-current-sink-mute
   "<XF86AudioMicMute>"     #'pulseaudio-control-toggle-current-source-mute))
