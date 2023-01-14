(evil-collection-inhibit-insert-state 'wpa-mode-map)
(evil-set-initial-state 'wpa-mode-map 'normal)
(evil-collection-define-key 'normal 'wpa-mode-map
  "s" 'wpa-scan
  "r" 'revert-buffer
  "c" 'wpa-connect
  (kbd "RET") 'wpa-connect)
