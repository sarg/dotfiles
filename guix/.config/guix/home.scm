(use-modules
  (guix transformations)
  (guix gexp)
  (gnu home)
  (gnu home services)
  (gnu home services shepherd)
  (gnu services)
  (gnu packages)
  (gnu home services shells))

(define %user "sarg")

(define S specification->package)

(home-environment
 (packages
  (map specification->package '("aria2" "atool" "readymedia")))

 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (guix-defaults? #t)
          (bash-profile (list (plain-file "bash_profile"
                                          "[[ ! $DISPLAY && $XDG_VTNR -eq 1 ]] && exec $HOME/start.sh")))))

        (simple-service 'minidlna-config
                        home-files-service-type
                        (list (list "config/minidlna.conf"
                                    (mixed-text-file "minidlna.conf"
                                                     "media_dir=V,/home/" %user "/Movies/\n"
                                                     "db_dir=/home/" %user "/.cache/minidlna/\n"
                                                     "log_dir=/home/" %user "/.cache/minidlna/\n"
                                                     "wide_links=yes"))))

        (service home-shepherd-service-type
                 (home-shepherd-configuration
                  (services
                   (list
                    (shepherd-service
                     (documentation "Run minidlnad")
                     (provision '(minidlnad))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append (S "readymedia") "/sbin/minidlnad")
                                     "-d" "-f" "/home/sarg/.config/minidlna.conf")))
                     (stop #~(make-kill-destructor)))))))

        (simple-service 'additional-env-vars-service
                        home-environment-variables-service-type
                        `(("PATH" . "$HOME/.local/bin:$PATH")
                          ("VISUAL" . "emacsclient")
                          ("EDITOR" . "emacsclient"))))))
