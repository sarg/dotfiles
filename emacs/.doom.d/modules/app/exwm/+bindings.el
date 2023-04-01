(defun exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (if (vectorp key) key (kbd key))
                        command)
    (setq key     (pop bindings)
          command (pop bindings))))

(defun sarg/run-or-raise (NAME PROGRAM &rest ARGS)
  (interactive)
  (let ((buf (cl-find-if
              (lambda (buf) (string= NAME (buffer-name buf)))
              (buffer-list))))

    (if buf (switch-to-buffer buf)
      (apply #'start-process
             (append (list NAME nil "setsid" "-w" PROGRAM) ARGS)))))

(setq exwm-input-prefix-keys
        `(,(kbd exwm-leader-key)
          ?\C-w ; window movements
          ?\C-x
          ?\M-x
          ?\M-m
          ?\C-g
          ?\C-m
          ?\C-h
          ?\C-р                         ; cyrillic
          ))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
(define-key exwm-mode-map [?\C-c] 'nil)

(setq exwm-input-resize-event 'C-down-mouse-3
      exwm-input-move-event 'C-down-mouse-1)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.

(setq exwm-input-simulation-keys
(mapcar (lambda (c) (cons (kbd (car c)) (cdr c)))
        `(
                ;; ("C-b" . left)
                ;; ("C-f" . right)
                ;; ("C-p" . up)
                ("C-m" . return)
                ;; ("C-n" . down)
                ("DEL" . backspace)
                ("C-р" . backspace))))


(exwm-bind-command
 "<f13> r"     #'app-launcher-run-app

 "<f13> h"     #'evil-window-left
 "<f13> j"     #'evil-window-down
 "<f13> k"     #'evil-window-up
 "<f13> l"     #'evil-window-right
 "<f13> e"    `(lambda () (interactive)
                 (sarg/run-or-raise "qutebrowser" "qutebrowser")
                 (exwm-workspace-switch (exwm-workspace-name-to-index "brow"))))
