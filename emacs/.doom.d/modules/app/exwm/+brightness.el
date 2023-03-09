(defhydra hydra-brightness ()
  ""
  ("<next>" (backlight-inc 500))
  ("<prior>" (backlight-dec 500))
  ("RET" (message "OK") :exit t))

(exwm-bind-command
 "<XF86MonBrightnessUp>"   `backlight
 "<XF86MonBrightnessDown>" `backlight)
