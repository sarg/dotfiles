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
              (default-cache-ttl-ssh 86400)
              (max-cache-ttl-ssh 86400)
              (extra-content "disable-scdaemon")))
            (simple-service 'cyrus-vars home-environment-variables-service-type
              `(("SASL_PATH" . "$HOME/.guix-home/profile/lib/sasl2")))
            
            (simple-service
             'goimapnotify
             home-shepherd-service-type
             (list
              (shepherd-service
               (provision '(goimapnotify))
               (requirement '(x11-display gpg-agent))
               (modules `(((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                          ,@(@ (gnu services shepherd) %default-modules)))
               (auto-start? #f)
               (start #~(make-forkexec-constructor
                         (list #$(file-append (@ (gnu packages mail) goimapnotify) "/bin/goimapnotify"))
                         #:log-file (string-append %user-log-dir "/goimapnotify.log")))
               (stop #~(make-kill-destructor)))))
            (simple-service 'direnv-bash-hook home-bash-service-type
             (home-bash-extension
              (bashrc (list
                       (plain-file "envrc-hook"
                        "eval \"$(direnv hook bash)\"")))))
            (simple-service 'eat-bash-integration home-bash-service-type
             (home-bash-extension
              (bashrc (list
                       (plain-file "eat"
                        "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")))))))
 (packages
  (map (compose list specification->package+output)
       '(
         "emacs-debbugs" "gnu-standards" "mumi-with-jaro"
         "emacs-next"
         "emacs-telega" "emacs-telega-contrib"
         "emacs-vterm" "guix-icons"
         
         "tree-sitter-typescript"
         "tree-sitter-javascript"
         "tree-sitter-json"
         "tree-sitter-html"
         "tree-sitter-css"
         "tree-sitter-bash"
         "tree-sitter-python"
         "tree-sitter-clojure"
         
         ; modules dependencies
         "shfmt" "shellcheck" "fd"
         
         ; dirvish
         "ffmpegthumbnailer" "poppler" "mediainfo" "vips"
         "emacs-exwm"
         "emacs-exwm-ss"
         "emacs-ednc"
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
         "emacs-elfeed-org"
         "emacs-lexic" "sdcv" "emacs-google-translate"
         "mu" "isync" "msmtp" "emacs-consult-mu" "oama" "cyrus-sasl-xoauth2"
         "emacs-mpv" "emacs-webpaste" "emacs-yeetube"
         "emacs-eat" "emacs-detached" "direnv"
         "emacs-dired-hacks" "emacs-dired-du"
         "emacs-openwith" "emacs-sxiv"
         "emacs-bookmark-plus"
         ;; "emacs-gpx"
         "emacs-osm"
         "emacs-org-node"))))
