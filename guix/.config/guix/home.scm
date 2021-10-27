(use-modules
  (guix transformations)
  (guix gexp)
  (gnu home)
  (gnu home services)
  (gnu services)
  (gnu packages)
  (gnu home services shells))

(home-environment
 (packages
  (map specification->package '("aria2" "atool")))

 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (guix-defaults? #t)
          (bash-profile (list (plain-file "bash_profile" "\
if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec $HOME/start.sh
fi")))))

        (simple-service 'additional-env-vars-service
                        home-environment-variables-service-type
                        `(("PATH" . "$HOME/.local/bin:$PATH")
                          ("VISUAL" . "emacsclient")
                          ("EDITOR" . "emacsclient"))))))
