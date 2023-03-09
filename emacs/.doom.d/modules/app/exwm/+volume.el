(use-package! pulseaudio-control
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "5%"
        pulseaudio-control--current-source "@DEFAULT_SOURCE@"))
