(use-modules
 (gnu packages)
 (gnu home))
(home-environment
 (services (list
            (simple-service 'doom-vars home-environment-variables-service-type
              `(("DOOMLOCALDIR" . "$HOME/.local/doom/")
                ("DOOMDIR" . "$HOME/.dotfiles/emacs/.doom.d/")))
            (simple-service 'doom home-files-service-type
              `((".local/bin/doomemacs"
                 ,((@ (personal services utils) chmod-computed-file)
                   (mixed-text-file "doomemacs" "emacs --init-directory=" (specification->package "doomemacs") " $@")
                   #o555))))
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
         "emacs-pinentry"
         "recordmydesktop" "ffmpeg"
         "emacs-org-modern"
         "emacs-calibredb" "sqlite" "emacs-nov-el" "emacs-pdf-tools"
         "emacs-password-generator"
         "emacs-password-store"
         "emacs-password-store-otp"
         "emacs-emms"
         "emacs-lexic" "sdcv"
         "emacs-mpv" "emacs-webpaste" "emacs-yeetube"
         "emacs-eat" "emacs-detached"
         "emacs-dired-hacks" "emacs-dired-du" "avfs"
         "emacs-dired-git-info" "emacs-openwith" "emacs-sxiv"
         "emacs-bookmark-plus"
         "emacs-gpx" "emacs-osm"))))
