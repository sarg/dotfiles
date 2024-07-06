(define-module (personal services minidlna)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp))

(define minidlna-service
  (shepherd-service
   (documentation "Run minidlnad")
   (provision '(minidlnad))
   (modules '((shepherd support)))      ;for 'user-homedir'
   (start #~(make-forkexec-constructor
             (list #$(file-append (@ (gnu packages upnp) readymedia) "/sbin/minidlnad")
                   "-d" "-P" "/var/run/minidlna.pid" "-f"
                   #$(mixed-text-file "minidlna.conf"
                                      "media_dir=" #~user-homedir "/Movies/\n"
                                      "db_dir=" #~user-homedir "/.cache/minidlna/\n"
                                      "log_dir=" #~user-homedir "/.cache/minidlna/\n"
                                      "wide_links=yes"))))
   (stop #~(make-kill-destructor))))
