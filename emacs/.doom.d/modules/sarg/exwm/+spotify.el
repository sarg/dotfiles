(defun spotify-set-mute (val)
  (let ((sink-input (-first (lambda (el)
                              (string= "spotify" (alist-get "application.process.binary" (cdr el) nil nil #'string=)))
                            (pulseaudio-control--get-sink-inputs))))
    (when sink-input
      (pulseaudio-control--set-sink-input-mute (car sink-input) val))))

(defun sarg/spotify-cut-ads (interface properties &rest ignored)
  (let* ((metadata (cl-caadr (assoc "Metadata" properties)))
         (url (cl-caadr (assoc "xesam:url" metadata))))
    (spotify-set-mute (string-prefix-p "https://open.spotify.com/ad/" url))))

(defun sarg/name-owner-changed-handler (interface properties &rest args)
  (when (string= "org.mpris.MediaPlayer2.spotify" interface)
    (if (string-empty-p properties)
        (setq sarg/spotify-ads-dbus-handler
              (dbus-register-signal :session "org.mpris.MediaPlayer2.spotify"
                                    "/org/mpris/MediaPlayer2"
                                    "org.freedesktop.DBus.Properties"
                                    "PropertiesChanged"
                                    #'sarg/spotify-cut-ads))
      (when sarg/spotify-ads-dbus-handler
        (dbus-unregister-object sarg/spotify-ads-dbus-handler)
        (setq sarg/spotify-ads-dbus-handler nil)))))

(use-package! spotify
  :config

  (setq sarg/name-owner-changed
        (dbus-register-signal :session
                              "org.freedesktop.DBus"
                              "/org/freedesktop/DBus"
                              "org.freedesktop.DBus"
                              "NameOwnerChanged"
                              #'sarg/name-owner-changed-handler))
  ;; (dbus-unregister-object sarg/spotify-ads-dbus-handler)

  (spacemacs/exwm-bind-command
   "<XF86AudioPlay>"    #'emms-pause
   "<XF86AudioNext>"    #'emms-next
   "<XF86AudioPrev>"    #'emms-previous))

(use-package! counsel-spotify
  :config

  (cl-defmethod counsel-spotify-do-play ((backend counsel-spotify-linux-backend) (playable counsel-spotify-playable))
    (emms-add-url (uri playable))
    (emms-playlist-current-select-last)
    (emms-stop) (emms-start))

  (advice-add 'counsel-spotify-verify-credentials :before
              (lambda ()
                (when (string= counsel-spotify-client-secret "")
                  (setq counsel-spotify-client-id (+pass-get-field "Sites/spotify.com" "client-id")
                        counsel-spotify-client-secret (+pass-get-field "Sites/spotify.com" "client-secret"))))))


(defun emms-player-spotify-info-meta-update-track (interface properties &rest ignored)
    (let* ((metadata (cl-caadr (assoc "Metadata" properties)))
           (album (cl-caadr (assoc "xesam:album" metadata)))
           (duration (cl-caadr (assoc "mpris:length" metadata)))
           (artist (cl-caaadr (assoc "xesam:artist" metadata)))
           (title (cl-caadr (assoc "xesam:title" metadata)))
           (trackid (cl-caadr (assoc "xesam:trackid" metadata)))
           (current-emms-track (emms-playlist-current-selected-track)))

      (when (and current-emms-track
                 (eq emms-player-playing-p emms-player-spotify))
        (emms-track-set current-emms-track 'info-artist artist)
        (emms-track-set current-emms-track 'info-album album)
        (emms-track-set current-emms-track 'info-title title)
        (emms-track-set current-emms-track 'info-playing-time (round (/ (float duration) 1000 1000)))
        (emms-track-updated current-emms-track))))

(defun emms-player-spotify-playable-p (track)
  (and (memq (emms-track-type track) '(url))
       (string-match-p (emms-player-get emms-player-spotify 'regex) (emms-track-name track))))

(defun emms-player-spotify--transform-url (url)
  (or (and (string-prefix-p "https" url)
           (concat "spotify"
                   (replace-regexp-in-string
                    "/" ":"
                    (car (url-path-and-query (url-generic-parse-url url))))))
      url))

(defun emms-player-spotify-start (track)
  (emms-player-spotify-enable-dbus-handler)
  (dbus-call-method-asynchronously :session
                                   "org.mpris.MediaPlayer2.spotify"
                                   "/org/mpris/MediaPlayer2"
                                   "org.mpris.MediaPlayer2.Player"
                                   "OpenUri"
                                   nil
                                   (emms-player-spotify--transform-url (emms-track-name track)))
  (emms-player-started emms-player-spotify))

(defun emms-player-spotify-stop ()
  (spotify-pause)
  (emms-player-spotify-disable-dbus-handler)
  (emms-player-stopped))

(defun emms-player-spotify-disable-dbus-handler ()
  (dbus-unregister-object (emms-player-get emms-player-spotify 'dbus-handler)))

(defun emms-player-spotify-enable-dbus-handler ()
  (emms-player-set emms-player-spotify
                   'dbus-handler
                   (dbus-register-signal :session
                                         "org.mpris.MediaPlayer2.spotify"
                                         "/org/mpris/MediaPlayer2"
                                         "org.freedesktop.DBus.Properties"
                                         "PropertiesChanged"
                                         #'emms-player-spotify-info-meta-update-track)))

;(emms-add-url "spotify:user:spotify:playlist:37i9dQZEVXbk5YUFhWd7TC")
;(emms-add-url "spotify:artist:3SYkxKBdwKFCTxWDh9l5f9")

(use-package! emms
  :config

  (defcustom emms-player-spotify
    (emms-player
     #'emms-player-spotify-start
     #'emms-player-spotify-stop
     #'emms-player-spotify-playable-p)
    "*Parameters for spotify player."
    :type '(cons symbol alist)
    :group 'emms-player-spotify)

  (emms-player-set emms-player-spotify 'regex
                   (rx string-start (or "https://open.spotify.com" "spotify:")))
  (emms-player-set emms-player-spotify 'pause #'spotify-pause)
  (emms-player-set emms-player-spotify 'resume #'spotify-play)

  (emms-all)
  (emms-history-load)
  (setq emms-player-list (list emms-player-spotify emms-player-mpv)
        emms-playlist-buffer-name "*Music*"
        emms-source-file-default-directory (expand-file-name "~/Music")
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-browser-covers 'emms-browser-cache-thumbnail))
