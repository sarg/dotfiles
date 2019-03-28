(defun spotify-set-mute (val)
  (let ((sink-input (-first (lambda (el)
                              (string= "Spotify" (alist-get "application.name" (cdr el) nil nil #'string=)))
                            (pulseaudio-control--get-sink-inputs))))
    (when sink-input
      (pulseaudio-control--set-sink-input-mute (car sink-input) val))))

(defun sarg/spotify-cut-ads (interface properties &rest ignored)
  (let* ((metadata (cl-caadr (assoc "Metadata" properties)))
         (url (cl-caadr (assoc "xesam:url" metadata))))
    (spotify-set-mute (string-prefix-p "https://open.spotify.com/ad/" url))))

(def-package! spotify
  :config

  (dbus-register-signal :session
                        "org.mpris.MediaPlayer2.Player"
                        "/org/mpris/MediaPlayer2"
                        "org.freedesktop.DBus.Properties"
                        "PropertiesChanged"
                        #'sarg/spotify-cut-ads)

  (spacemacs/exwm-bind-command
   "<XF86AudioPlay>"    #'spotify-playpause
   "<XF86AudioNext>"    #'spotify-next
   "<XF86AudioPrev>"    #'spotify-previous))
