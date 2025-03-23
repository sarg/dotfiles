;; generated from config.org
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
            (service home-gpg-agent-service-type
             (home-gpg-agent-configuration
              (pinentry-program
               (file-append pinentry-emacs "/bin/pinentry-emacs"))
              (ssh-support? #t)
              (default-cache-ttl 86400)
              (max-cache-ttl 86400)
              (extra-content "disable-scdaemon")))
            (simple-service 'cyrus-vars home-environment-variables-service-type
              `(("SASL_PATH" . "$HOME/.guix-home/profile/lib/sasl2")))
            (simple-service 'eat-bash-integration home-bash-service-type
             (home-bash-extension
              (bashrc (list
                       (plain-file "eat"
                        "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")))))))
 (packages
  (map (compose list specification->package+output)
       '(
         "emacs-debbugs" "gnu-standards" "mumi-with-jaro" "emacs-guix"
         "emacs-next"
         "emacs-telega" "emacs-telega-contrib"
         "emacs-vterm"
         
         ; modules dependencies
         "shfmt" "shellcheck" "fd"
         "emacs-exwm"
         "emacs-exwm-ss"
         "emacs-ednc"
         "emacs-mini-echo"
         "dex"
         "emacs-catppuccin-theme"
         "jaro"
         "darkman"
         "redshift"
         "emacs-bluetooth"
         "emacs-discomfort"
         "emacs-pinentry" "gnupg"
         "recordmydesktop"
         "font-iosevka-curly" ; for org-modern bullets
         "emacs-calibredb" "sqlite" "emacs-nov-el" "emacs-pdf-tools"
         "password-store"
         "pass-otp"
         "emacs-password-generator"
         "emacs-password-store"
         "emacs-password-store-otp"
         "emacs-emms"
         "emacs-elfeed-org"
         "emacs-lexic" "sdcv" "emacs-google-translate"
         "mu" "isync" "msmtp" "emacs-consult-mu" "oama" "cyrus-sasl-xoauth2"
         "emacs-mpv" "emacs-webpaste" "emacs-yeetube"
         "emacs-eat" "emacs-detached"
         "emacs-dired-hacks" "emacs-dired-du"
         "emacs-dired-git-info" "emacs-openwith" "emacs-sxiv"
         "emacs-bookmark-plus"
         ;; "emacs-gpx"
         "emacs-osm"
         "emacs-org-node"))))
