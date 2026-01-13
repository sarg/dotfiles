;;; build-profile.el --- Generates doom's startup file. -*- lexical-binding: t; -*-
(require 'doom-lib)
(letf! (defun doom-initialize-core-packages (&optional force-p)
         (message "Inhibiting core packages initialization"))

  ;; this thing generates autoloads
  ;; in guix they're collected by site-start.el
  (setq doom-profile-generators
        (cl-delete "90-loaddefs-packages.auto.el"
                   doom-profile-generators
                   :key 'car :test 'string=))

  (doom-modules-initialize)
  (setq doom-packages (doom-package-list))
  (let ((missing-pkgs '()))
    (dolist (package doom-packages)
      (cl-destructuring-bind
          (name &key disable ignore &allow-other-keys) package

        (when disable
          (cl-pushnew name doom-disabled-packages))
        (unless (or disable ignore (eq 'straight name) (locate-library (symbol-name name)))
          (cl-pushnew (concat "\"emacs-" (symbol-name name) "\"") missing-pkgs))))
    (when missing-pkgs
      (message "!!!!! MISSING PACKAGES !!!!!")
      (mapc 'message (sort missing-pkgs))
      (setq backtrace-on-error-noninteractive nil)
      (error "Please add missing packages to inputs of doomemacs-configuration")))

  (doom-profile-generate))
