(setq exwm-workspace-number 2)
;;; quick switch to last
;; Quick switching between workspaces
(defvar exwm-toggle-workspace 0
  "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")

(defun exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))

(advice-add 'exwm-workspace-switch :before
            (lambda (&rest r)
              (setq exwm-toggle-workspace exwm-workspace-current-index)))

(exwm-input-set-key (kbd "<f13> TAB") #'exwm-jump-to-last-exwm)

;;; shortcuts
;; + Set shortcuts to switch to a certain workspace.
;; use all digits, so that if new workspace created it could be switched to
(dotimes (i 9)
  (exwm-input-set-key (kbd (format "<f13>%d" (1+ i)))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch ,i))))

;;; EWMH integration with names support
(defvar exwm-workspace-names '())

(require 'cl-seq)
(defsubst exwm-workspace-name-to-index (name)
  "Returns workspace index by NAME."
  (cl-position name exwm-workspace-names :test #'equal))

(setq exwm-workspace-index-map
      (lambda (index)
        (if (< index (length exwm-workspace-names))
            (elt exwm-workspace-names index)
          (number-to-string index))))

(defun exwm-workspace--update-ewmh-desktop-names ()
  (xcb:+request exwm--connection
      (make-instance 'xcb:ewmh:set-_NET_DESKTOP_NAMES
                     :window exwm--root :data
                     (mapconcat (lambda (i) (funcall exwm-workspace-index-map i))
                                (number-sequence 0 (1- (exwm-workspace--count)))
                                "\0"))))

(add-hook 'exwm-workspace-list-change-hook
          #'exwm-workspace--update-ewmh-desktop-names)
