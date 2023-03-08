(evil-collection-inhibit-insert-state 'wpa-manager-mode-map)
(evil-set-initial-state 'wpa-manager-mode-map 'normal)
(evil-collection-define-key 'normal 'wpa-manager-mode-map
  "s" 'wpa-manager-scan
  "r" 'revert-buffer
  "c" 'wpa-manager-connect
  (kbd "RET") 'wpa-manager-connect)
