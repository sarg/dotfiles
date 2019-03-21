(defun spotify-set-mute (val)
  (let ((sink-input (-first (lambda (el)
                              (string= "\"Spotify\"" (alist-get "application.name" (cdr el) nil nil #'string=)))
                            (pulseaudio-control--get-sink-inputs))))
    (when sink-input
      (pulseaudio-control--set-sink-input-mute (car sink-input) (if val "1" "0")))))

(defun sarg/spotify-cut-ads (interface properties &rest ignored)
  (let* ((metadata (cl-caadr (assoc "Metadata" properties)))
         (url (cl-caadr (assoc "xesam:url" metadata))))
    (spotify-set-mute (string-prefix-p "https://open.spotify.com/ad/" url))))

(defun pulseaudio-control--get-sink-inputs ()
  "Internal function; get a list of Pulse sinks via `pactl'."
  (let ((sink-inputs '())
        prop-begin prop-end sep)
    (with-temp-buffer
      (erase-buffer)
      (pulseaudio-control--call-pactl "list sink-inputs")
      (goto-char (point-min))
      (while (re-search-forward "^Sink Input #\\([[:digit:]]+\\)$" nil t)
        (setq-local input-id (match-string 1))
        (setq-local props '())

        (re-search-forward "^\tProperties:$")
        (while (and
                (= (forward-line 1) 0)
                (search-forward " = " nil t)
                (progn
                  (setq sep (point))
                  (back-to-indentation)
                  (setq prop-begin (buffer-substring-no-properties (point) (- sep 3)))
                  (end-of-line)
                  (setq prop-end (buffer-substring-no-properties sep (point)))
                  (push (cons prop-begin prop-end) props)
                  t)))

        (push (cons input-id props) sink-inputs)))
    sink-inputs))

(defun pulseaudio-control--set-sink-input-mute (id val)
  "Set mute status for sink-input with ID to VAL
If VAL is string it is passed to set-sink-input-mute as is.
Otherwise t means mute (\"1\") and nil - unmute (\"0\")."
  (pulseaudio-control--call-pactl
   (concat "set-sink-input-mute " id " "
           (or (-first (lambda (x) (string= x val)) '("1" "0" "toggle"))
               (if val "1" "0")))))

(def-package! spotify
  :config

  (spotify-enable-song-notifications)
  (advice-add 'spotify-properties-changed :before #'sarg/spotify-cut-ads)

  (spacemacs/exwm-bind-command
   "<XF86AudioPlay>"   #'spotify-playpause
   "<XF86AudioNext>"    #'spotify-next
   "<XF86AudioPrev>"    #'spotify-previous))
