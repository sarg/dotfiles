;;; net-usage.el --- A mode-line segment with net usage stats -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; set net-usage-interface to the name of the interface to monitor
;; start M-x net-usage-mode
;; add net-usage-mode-line-string to e.g. global-mode-string
;; every 5 seconds the segment would be updated
;; to reset stats M-x net-usage-reset
;;
;;; Code:
(require 'files)

(defgroup net-usage nil
  "Networking stats in modeline."
  :group 'communication
  :prefix 'net-usage)

(defcustom net-usage-interface nil
  "Network interface to monitor."
  :group 'net-usage
  :type 'string)

(defvar net-usage-mode-line-format " ↓%r ↑%t "
  "Format-spec to display net stats.
Use %r for rx bytes, %t for tx bytes.")

(defvar net-usage-mode-line-string nil
  "Modeline string showing network stats.")

(defvar net-usage--timer nil
  "Timer for periodic network stats update.")

(defvar net-usage--baseline '(0 . 0)
  "Initial measurement point - (rx . tx).")

(defun net-usage--read-stats (iface)
  "Read current RX/TX bytes of IFACE."
  (when-let* ((p (concat "/sys/class/net/" iface "/statistics/"))
              ((file-directory-p p)))
    (with-temp-buffer
      (insert-file-contents (concat p "tx_bytes"))
      (insert-file-contents (concat p "rx_bytes"))
      (cons (read (current-buffer))
            (read (current-buffer))))))

(defun net-usage--update ()
  "Update network usage statistics."
  (when-let* ((stats (net-usage--read-stats net-usage-interface))
              (rx (- (car stats) (car net-usage--baseline)))
              (tx (- (cdr stats) (cdr net-usage--baseline))))
    (setq net-usage-mode-line-string
          (format-spec net-usage-mode-line-format
                       `((?r . ,(file-size-human-readable rx))
                         (?t . ,(file-size-human-readable tx)))))
    (force-mode-line-update)))

(defun net-usage-reset ()
  "Reset counters to zero."
  (interactive)
  (setq net-usage--baseline (net-usage--read-stats net-usage-interface))
  (net-usage--update))

;;;###autoload
(define-minor-mode net-usage-mode
  "Global minor mode to display network usage in modeline."
  :global t
  :group 'netstat
  :lighter nil
  (cond
   ((not net-usage-mode)
    (when net-usage--timer
      (cancel-timer net-usage--timer)
      (setq net-usage-mode-line-string nil)
      (setq net-usage--timer nil)))

   ((null net-usage-interface)
    (setq net-usage-mode nil)
    (user-error "Please set `net-usage-interface'"))

   (t
    (setq net-usage--timer (run-with-timer 0 5 #'net-usage--update)))))

(provide 'net-usage)
;;; net-usage.el ends here
