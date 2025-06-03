(define-module (personal services secretsd)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages password-utils))

(define (secretsd-shepherd-service key)
  "Return a <shepherd-service> for secretsd with CONFIG."

  (list (shepherd-service
         (documentation "secretsd backend")
         (provision '(secrets))
         (auto-start? #f)
         (respawn? #f)
         (modules `(((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                    ,@%default-modules))
         (start #~(make-forkexec-constructor
                   (list (string-append #$secretsd "/bin/secretsd"))
                   #:environment-variables
                   (cons*
                    #$@(if key
                           (list (string-append "SECRETSD_KEY=" key))
                           '())

                    (default-environment-variables))
                   #:log-file (string-append %user-log-dir "/secretsd.log")))
         (stop #~(make-kill-destructor)))))

(define secretsd-dbus-service
  (package
    (name "secretsd-profile-package")
    (version "0")
    (source
     (mixed-text-file
      "secrets.service"
      "[D-BUS Service]\nName=org.freedesktop.secrets\nExec=/bin/sh -c 'herd start secrets'"))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((services (string-append #$output "/share/dbus-1/services")))
            (mkdir-p services)
            (symlink #$source (string-append services "/org.freedesktop.secrets.service"))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public secretsd-service-type
  (service-type
   (name 'secretsd)
   (description "run secretsd")
   (extensions
    (list (service-extension home-shepherd-service-type
                             secretsd-shepherd-service)

          (service-extension home-profile-service-type
                             (const (list secretsd-dbus-service)))))
   (default-value #f)))
