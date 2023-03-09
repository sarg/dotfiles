(use-package! backlight
  :config
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'backlight)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'backlight))
