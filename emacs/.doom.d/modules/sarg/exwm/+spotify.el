(def-package! spotify
  :config

  (spacemacs/exwm-bind-command
   "<XF86AudioPlay>"   #'spotify-playpause
   "<XF86AudioNext>"    #'spotify-next
   "<XF86AudioPrev>"    #'spotify-previous))
