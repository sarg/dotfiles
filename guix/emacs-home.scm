(use-modules
 (gnu packages)
 (gnu home))
(home-environment
 (services (list
            (simple-service 'doom-vars home-environment-variables-service-type
              `(("DOOMLOCALDIR" . "$HOME/.local/doom/")
                ("DOOMDIR" . "$HOME/.dotfiles/emacs/.doom.d/")
                ("VISUAL" . "emacsclient")
                ("EDITOR" . "emacsclient")))
            (simple-service 'doom home-files-service-type
              `((".local/bin/doomemacs"
                 ,((@ (personal services utils) chmod-computed-file)
                   (mixed-text-file "doomemacs" "emacs --init-directory=" (specification->package "doomemacs") " $@")
                   #o555))))
            (service home-xdg-mime-applications-service-type
             (home-xdg-mime-applications-configuration
              (default '((x-scheme-handler/org-protocol . org-protocol.desktop)
                         (x-scheme-handler/mailto . emacsmail.desktop)))
              (desktop-entries
               (list
                (xdg-desktop-entry
                 (file "calibre")
                 (name "Calibre E-Book Viewer")
                 (type 'application)
                 (config `((exec . ,(file-append (specification->package "calibre") "/bin/calibre")))))
                (xdg-desktop-entry
                 (file "emacsmail")
                 (name "Emacs Mail client")
                 (type 'application)
                 (config '((exec . "emacs-mail %u")
                           (hidden . "true"))))
                (xdg-desktop-entry
                 (file "org-protocol")
                 (name "org-protocol")
                 (type 'application)
                 (config '((exec . "emacsclient %u")
                           (hidden . "true"))))))))
            (service home-gpg-agent-service-type
             (home-gpg-agent-configuration
              (pinentry-program
               (file-append pinentry-emacs "/bin/pinentry-emacs"))
              (ssh-support? #t)
              (default-cache-ttl 86400)
              (max-cache-ttl 86400)))
            (simple-service 'eat-bash-integration home-bash-service-type
             (home-bash-extension
              (bashrc (list
                       (plain-file "eat"
                        "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")))))))
 (packages
  (map (compose list specification->package+output)
       '(
         "emacs-debbugs" "gnu-standards" "mumi" "emacs-guix"
         "emacs-next"
         "emacs-telega" "emacs-telega-contrib"
         "emacs-vterm"
         "emacs-exwm"
         "emacs-exwm-ss"
         "dex"
         "emacs-bluetooth"
         "emacs-discomfort"
         "emacs-dracula-theme"
         "emacs-doom-themes"
         "emacs-circadian"
         "emacs-pinentry" "gnupg"
         "recordmydesktop" "ffmpeg"
         "emacs-org-modern"
         "emacs-calibredb" "sqlite" "emacs-nov-el" "emacs-pdf-tools"
         "password-store"
         "pass-otp"
         "emacs-password-generator"
         "emacs-password-store"
         "emacs-password-store-otp"
         "emacs-emms"
         "emacs-lexic" "sdcv"
         "mu" "isync" "msmtp"
         "emacs-mpv" "emacs-webpaste" "emacs-yeetube"
         "emacs-eat" "emacs-detached"
         "emacs-dired-hacks" "emacs-dired-du" "avfs"
         "emacs-dired-git-info" "emacs-openwith" "emacs-sxiv"
         "emacs-bookmark-plus"
         "emacs-gpx" "emacs-osm"))))
