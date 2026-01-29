;;; zathura-sync-theme.el --- Synchronize Zathura's look and feel with Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Amol Vaidya

;; Author: Amol Vaidya
;; Version: 20240710.1000
;; Keywords: faces
;; URL: https://github.com/amolv06/zathura-sync-theme
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; This package is inspired by the blog post at
;; https://blog.akaisuisei.org/communicating-with-zathura-via-dbus.html
;; written by mafty.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(defgroup zathura-sync-theme nil
  "Synchronize Zathura's look and feel with Emacs."
  :prefix "zathura-sync-theme-"
  :group 'applications)

(defcustom zathura-sync-theme-config-file "~/.config/zathura/theme"
  "Config location to put colors into."
  :type 'file
  :group 'zathura-sync-theme)

(defun zathura-sync-theme--write-config ()
  "Overwrites theme config."
  (with-temp-file zathura-sync-theme-config-file
    (let ((fg (face-attribute 'default :foreground nil 'default))
          (bg (face-attribute 'default :background nil 'default)))
      (insert "# synced with emacs theme by zathura-sync-theme"
              "\nset recolor-darkcolor \\" fg
              "\nset recolor-lightcolor \\" bg
              "\nset default-fg \\" fg
              "\nset default-bg \\" bg
              "\nset statusbar-bg \\" bg
              "\nset statusbar-fg \\" fg
              "\nset recolor true"))))

(defun zathura-sync-theme--set (&rest _args)
  "Writes theme config and sends Zathura D-Bus command to refresh it."
  (let ((zathura-services (cl-remove-if-not (lambda (x) (cl-search "zathura" x))
					    (dbus-list-names :session)))
	(zathura-path "/org/pwmt/zathura")
	(zathura-interface "org.pwmt.zathura")
	(zathura-method "SourceConfig"))

    (zathura-sync-theme--write-config)
    (dolist (svc zathura-services)
      (dbus-call-method-asynchronously :session
                                       svc
                                       zathura-path
                                       zathura-interface
                                       zathura-method
                                       nil))))

;;;###autoload
(define-minor-mode zathura-sync-theme-mode
  "Synchronize the look and feel of Zathura with Emacs."
  :global t
  :group 'zathura-sync-theme
  :init-value nil
  :lighter "Zathura"
  (cond
   (zathura-sync-theme-mode
    (zathura-sync-theme--write-config)
    (advice-add 'enable-theme :after #'zathura-sync-theme--set))

   (t
    (delete-file zathura-sync-theme-config-file)
    (advice-remove 'enable-theme #'zathura-sync-theme--set))))

(provide 'zathura-sync-theme)
;;; zathura-sync-theme.el ends here
