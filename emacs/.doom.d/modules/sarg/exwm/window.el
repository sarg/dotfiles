(defun fit-frame-to-buffer (&optional frame max-height min-height max-width min-width only)
  "Adjust size of FRAME to display the contents of its buffer exactly.
FRAME can be any live frame and defaults to the selected one.
Fit only if FRAME's root window is live.

MAX-HEIGHT, MIN-HEIGHT, MAX-WIDTH and MIN-WIDTH specify bounds on
the new total size of FRAME's root window.  MIN-HEIGHT and
MIN-WIDTH default to the values of `window-min-height' and
`window-min-width' respectively.  These arguments are specified
in the canonical character width and height of FRAME.

If the optional argument ONLY is `vertically', resize the frame
vertically only.  If ONLY is `horizontally', resize the frame
horizontally only.

The new position and size of FRAME can be additionally determined
by customizing the options `fit-frame-to-buffer-sizes' and
`fit-frame-to-buffer-margins' or setting the corresponding
parameters of FRAME."
  (interactive)
  (unless (fboundp 'display-monitor-attributes-list)
    (user-error "Cannot resize frame in non-graphic Emacs"))
  (setq frame (window-normalize-frame frame))
  (when (window-live-p (frame-root-window frame))
    (let* ((char-width (frame-char-width frame))
           (char-height (frame-char-height frame))
           ;; WINDOW is FRAME's root window.
           (window (frame-root-window frame))
           (line-height (window-default-line-height window))
           (parent (frame-parent frame))
           (monitor-attributes
            (unless parent
              (frame-monitor-attributes frame)))
           ;; FRAME'S parent or display sizes.  Used in connection
           ;; with margins.
           (geometry
            (unless parent
              (cdr (assq 'geometry monitor-attributes))))
           (parent-or-display-width
            (if parent
                (frame-native-width parent)
              (nth 2 geometry)))
           (parent-or-display-height
            (if parent
                (frame-native-height parent)
              (nth 3 geometry)))
           ;; FRAME's parent or workarea sizes.  Used when no margins
           ;; are specified.
           (parent-or-workarea
            (if parent
                `(0 0 ,parent-or-display-width ,parent-or-display-height)
              (cdr (assq 'workarea monitor-attributes))))
           ;; The outer size of FRAME.  Needed to calculate the
           ;; margins around the root window's body that have to
           ;; remain untouched by fitting.
           (outer-edges (frame-edges frame 'outer-edges))
           (outer-width (if outer-edges
                            (- (nth 2 outer-edges) (nth 0 outer-edges))
                          ;; A poor guess.
                          (frame-pixel-width frame)))
           (outer-height (if outer-edges
                             (- (nth 3 outer-edges) (nth 1 outer-edges))
                           ;; Another poor guess.
                           (frame-pixel-height frame)))
           ;; The text size of FRAME.  Needed to specify FRAME's
           ;; text size after the root window's body's new sizes have
           ;; been calculated.
           (text-width (frame-text-width frame))
           (text-height (frame-text-height frame))
           ;; WINDOW's body size.
           (body-width (window-body-width window t))
           (body-height (window-body-height window t))
           ;; The difference between FRAME's outer size and WINDOW's
           ;; body size.
           (outer-minus-body-width (- outer-width body-width))
           (outer-minus-body-height (- outer-height body-height))
           ;; The difference between FRAME's text size and WINDOW's
           ;; body size (these values "should" be positive).
           (text-minus-body-width (- text-width body-width))
           (text-minus-body-height (- text-height body-height))
           ;; The current position of FRAME.
           (position (frame-position frame))
           (left (car position))
           (top (cdr position))

           ;; The margins specified for FRAME.  These represent pixel
           ;; offsets from the left, top, right and bottom edge of the
           ;; display or FRAME's parent's native rectangle and have to
           ;; take care of the display's taskbar and other obstacles.
           ;; If they are unspecified, constrain the resulting frame
           ;; to its workarea or the parent frame's native rectangle.
           (margins (or (frame-parameter frame 'fit-frame-to-buffer-margins)
                        fit-frame-to-buffer-margins))
           ;; Convert margins into pixel offsets from the left-top
           ;; corner of FRAME's display or parent.
           (left-margin (if (nth 0 margins)
                            (window--sanitize-margin
                             (nth 0 margins) 0 parent-or-display-width)
                          (nth 0 parent-or-workarea)))
           (top-margin (if (nth 1 margins)
                           (window--sanitize-margin
                            (nth 1 margins) 0 parent-or-display-height)
                         (nth 1 parent-or-workarea)))
           (right-margin (if (nth 2 margins)
                             (- parent-or-display-width
                                (window--sanitize-margin
                                 (nth 2 margins) left-margin
                                 parent-or-display-width))
                           (+ (nth 0 parent-or-workarea) (nth 2 parent-or-workarea))))
           (bottom-margin (if (nth 3 margins)
                              (- parent-or-display-height
                                 (window--sanitize-margin
                                  (nth 3 margins) top-margin
                                  parent-or-display-height))
                            (+ (nth 1 parent-or-workarea) (nth 3 parent-or-workarea))))
           ;; Minimum and maximum sizes specified for FRAME.
           (sizes (or (frame-parameter frame 'fit-frame-to-buffer-sizes)
                      fit-frame-to-buffer-sizes))
           ;; Calculate the minimum and maximum pixel sizes of FRAME
           ;; from the values provided by the MAX-HEIGHT, MIN-HEIGHT,
           ;; MAX-WIDTH and MIN-WIDTH arguments or, if these are nil,
           ;; from those provided by `fit-frame-to-buffer-sizes'.
           (max-height
            (min
             (cond
              ((numberp max-height) (* max-height line-height))
              ((numberp (nth 0 sizes)) (* (nth 0 sizes) line-height))
              (t parent-or-display-height))
             ;; The following is the maximum height that fits into the
             ;; top and bottom margins.
             (max (- bottom-margin top-margin outer-minus-body-height))))
           (min-height
            (cond
             ((numberp min-height) (* min-height line-height))
             ((numberp (nth 1 sizes)) (* (nth 1 sizes) line-height))
             (t (window-min-size window nil nil t))))
           (max-width
            (min
             (cond
              ((numberp max-width) (* max-width char-width))
              ((numberp (nth 2 sizes)) (* (nth 2 sizes) char-width))
              (t parent-or-display-width))
             ;; The following is the maximum width that fits into the
             ;; left and right margins.
             (max (- right-margin left-margin outer-minus-body-width))))
           (min-width
            (cond
             ((numberp min-width) (* min-width char-width))
             ((numberp (nth 3 sizes)) (nth 3 sizes))
             (t (window-min-size window t nil t))))
           ;; Note: Currently, for a new frame the sizes of the header
           ;; and mode line may be estimated incorrectly
           (size
            (window-text-pixel-size window t t max-width max-height))
           (width (max (car size) min-width))
           (height (max (cdr size) min-height)))
      ;; Don't change height or width when the window's size is fixed
      ;; in either direction or ONLY forbids it.
      (cond
       ((or (eq window-size-fixed 'width) (eq only 'vertically))
        (setq width nil))
       ((or (eq window-size-fixed 'height) (eq only 'horizontally))
        (setq height nil)))
      ;; Fit width to constraints.
      (when width
        (unless frame-resize-pixelwise
          ;; Round to character sizes.
          (setq width (* (/ (+ width char-width -1) char-width)
                         char-width)))
        ;; The new outer width (in pixels).
        (setq outer-width (+ width outer-minus-body-width))
        ;; Maybe move FRAME to preserve margins.
        (let ((right (+ left outer-width)))
          (cond
           ((> right right-margin)
            ;; Move frame to left.
            (setq left (max left-margin (- left (- right right-margin)))))
           ((< left left-margin)
            ;; Move frame to right.
            (setq left left-margin)))))
      ;; Fit height to constraints.
      (when height
        (unless frame-resize-pixelwise
          (setq height (* (/ (+ height char-height -1) char-height)
                          char-height)))
        ;; The new outer height.
        (setq outer-height (+ height outer-minus-body-height))
        ;; Preserve margins.
        (let ((bottom (+ top outer-height)))
          (cond
           ((> bottom bottom-margin)
            ;; Move frame up.
            (setq top (max top-margin (- top (- bottom bottom-margin)))))
           ((< top top-margin)
            ;; Move frame down.
            (setq top top-margin)))))
      ;; Apply our changes.
      (setq text-width
            (if width
                (+ width text-minus-body-width)
              (frame-text-width frame)))
      (setq text-height
            (if height
                (+ height text-minus-body-height)
              (frame-text-height frame)))
      (modify-frame-parameters
       frame `((left . ,left) (top . ,top)
               (width . (text-pixels . ,text-width))
               (height . (text-pixels . ,text-height)))))))
