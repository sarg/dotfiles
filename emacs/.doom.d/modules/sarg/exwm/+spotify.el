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

(defun pulseaudio-control--get-sink-inputs ()
  "Internal function; get a list of Pulse sink inputs via `pactl'."
  (with-temp-buffer
    (let ((sink-inputs '())
          input-id props)

      (pulseaudio-control--call-pactl "list sink-inputs")
      (goto-char (point-min))

      (while (re-search-forward "^Sink Input #\\([[:digit:]]+\\)$" nil t)
        (setq input-id (match-string 1))
        (setq props '())

        (while (and
                (= (forward-line 1) 0)
                (or (and ; special case
                     (re-search-forward "^\t\s+balance \\(.+\\)$" (line-end-position) t)
                     (push (cons "balance" (match-string 1)) props))

                    (and ; line format
                     (re-search-forward "^\t\\([^:]+\\): \\(.+\\)$" (line-end-position) t)
                     (push (cons (match-string 1) (match-string 2)) props))
                    )))

        (re-search-forward "^\tProperties:$")
        (while (and
                (= (forward-line 1) 0)
                (re-search-forward "^\t\t\\([^=]+\\)\s=\s\"\\(.+\\)\"$" (line-end-position) t)
                (push (cons (match-string 1) (match-string 2)) props)))

        (push (cons input-id props) sink-inputs))
    sink-inputs)))

(defun pulseaudio-control--set-sink-input-mute (id val)
  "Set mute status for sink-input with ID to VAL.
nil or \"0\" - unmute
t or \"1\"   - mute
\"toggle\"   - toggle"
  (pulseaudio-control--call-pactl
   (concat "set-sink-input-mute " id " "
           (if (stringp val) val (if val "1" "0")))))

(def-package! spotify
  :config

  (spotify-enable-song-notifications)
  (advice-add 'spotify-properties-changed :override #'sarg/spotify-cut-ads)

  (spacemacs/exwm-bind-command
   "<XF86AudioPlay>"    #'spotify-playpause
   "<XF86AudioNext>"    #'spotify-next
   "<XF86AudioPrev>"    #'spotify-previous))
