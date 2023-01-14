;;; wpa-manager.el --- Manage wpa_supplicant via D-Bus interface
;;; Commentary:
;;; by default access to wpa_supplicant dbus is restricted to root only
;;; add this snippet to /etc/dbus-1
;;; <busconfig>
;;; <policy user='myuser'>
;;; <allow own='fi.w1.wpa_supplicant1'/>
;;; <allow send_destination='fi.w1.wpa_supplicant1'/>
;;; <allow send_interface='fi.w1.wpa_supplicant1'/>
;;; <allow receive_sender='fi.w1.wpa_supplicant1' receive_type='signal'/>
;;; </policy>
;;; </busconfig>
;;;
;;; Code:
(require 'dbus)
(require 'cl-lib)
(require 'dash)

(defconst wpa--service "fi.w1.wpa_supplicant1")
(defconst wpa--service-interface "fi.w1.wpa_supplicant1.Interface")
(defconst wpa--service-path "/fi/w1/wpa_supplicant1")
(defvar-local wpa--interface-path nil)

(defun wpa--dbus-get-props (object-path interface-name &optional prop)
  "Return all properties at OBJECT-PATH in INTERFACE-NAME.
If PROP is non-nil, return it only."
  (if prop
      (dbus-get-property :system wpa--service object-path (concat wpa--service interface-name) prop)
    (dbus-get-all-properties :system wpa--service object-path (concat wpa--service interface-name))))

(defun wpa--dbus-call (method callback &rest args)
  "Call dbus METHOD with CALLBACK and ARGS supplied."
  (apply #'dbus-call-method-asynchronously
   :system wpa--service wpa--interface-path wpa--service-interface method callback args))

(defun wpa--list-entries ()
  "List last-scanned access-points."
  (let ((current-bss (wpa--dbus-get-props wpa--interface-path ".Interface" "CurrentBSS"))
        (bss-list (wpa--dbus-get-props wpa--interface-path ".Interface" "BSSs")))

    (setq tabulated-list-entries
          (cl-loop for bss in bss-list
                   for props = (wpa--dbus-get-props bss ".BSS")
                   for ssid = (dbus-byte-array-to-string (cl-rest (assoc "SSID" props)))
                   for freq = (cl-rest (assoc "Frequency" props))
                   for signal = (cl-rest (assoc "Signal" props))
                   for bssid = (string-join (-map (lambda (n) (format "%02X" n)) (cl-rest (assoc "BSSID" props))) ":")
                   collect (list bss (vector
                                      (if (string= bss current-bss)
                                          (propertize ssid 'face 'bold)
                                        ssid)
                                      bssid
                                      (number-to-string freq)
                                      (number-to-string signal)))))))

(defun wpa-scan ()
  "Start scanning for access points."
  (interactive)
  (wpa--dbus-call "Scan"
                  (lambda () (message "Scan requested"))
                  '(:array (:dict-entry "Type" (:variant :string "active")))))

(defun wpa-select-network (network)
  "Select and connect to a NETWORK."
  (wpa--dbus-call "SelectNetwork" nil :object-path network))

(defun wpa-connect (pass)
  "Create network entry for the currently selected access point.
Connect to it using PASS."
  (interactive "MPassword: ")

  (let* ((bss (tabulated-list-get-id))
         (entry (assoc bss tabulated-list-entries))
         (ssid (aref (cadr entry) 0)))

    (wpa--dbus-call "AddNetwork" #'wpa-select-network
                    `(:array
                      (:dict-entry "ssid" (:variant :string ,ssid))
                      (:dict-entry "psk" (:variant :string ,pass))))))

(defvar-local wpa--scan-signal nil)
(define-derived-mode wpa-mode tabulated-list-mode
  "WPA Supplicant"
  "Major mode for managing WPA supplicant."
  (setq tabulated-list-format [("SSID" 24 t) ("BSSID" 32 t) ("Freq" 6 t) ("Signal" 6 t)]
        tabulated-list-entries nil
        tabulated-list-padding 0
        tabulated-list-sort-key (cons "Signal" nil))

  (setq-local wpa--interface-path
              (car (wpa--dbus-get-props wpa--service-path "" "Interfaces")))


  (setq wpa--scan-signal
        (dbus-register-signal :system wpa--service wpa--interface-path wpa--service-interface "ScanDone"
                              (lambda (success) (message "Scan finished: %s" success) (revert-buffer))))

  (add-hook 'tabulated-list-revert-hook #'wpa--list-entries nil t)
  (add-hook 'kill-buffer-hook (lambda () (dbus-unregister-object wpa--scan-signal)) nil t)

  (wpa--list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))

(defvar wpa-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?s] #'wpa-scan)
    (define-key map [?c] #'wpa-connect)
    map))

(defun wpa-manager ()
  "Manage wpa_supplicant."
  (interactive)
  (dbus-ping :system wpa--service 5000)
  (with-current-buffer (switch-to-buffer "*WPA Manager*")
    (wpa-mode))
  nil)

(provide 'wpa-manager)
;;; wpa-manager.el ends here
