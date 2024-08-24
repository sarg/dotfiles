;; from https://issues.guix.gnu.org/59971
(define-module (personal services iwd)
  #:export (iwd-configuration
            iwd-configuration?
            iwd-configuration-iwd
            iwd-configuration-openresolv
            iwd-configuration-coreutils
            iwd-configuration-config-file
            iwd-service-type)

  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system setuid)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages dns)
  #:use-module (gnu services admin)
  #:use-module (gnu services dbus)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)

  #:use-module (guix gexp)
  #:use-module (guix records))

(define-record-type* <iwd-configuration>
  iwd-configuration make-iwd-configuration
  iwd-configuration?
  (iwd iwd-configuration-iwd (default iwd))
  (coreutils coreutils-configuration-coreutils (default coreutils))
  (openresolv openresolv-configuration-openresolv (default openresolv))
  (config-file iwd-configuration-config-file
               (default (plain-file "iwd-main.conf"
                                    (string-join
                                     '("[General]"
                                       "EnableNetworkConfiguration=true"
                                       "[Network]"
                                       "NameResolvingService=resolvconf")
                                     "\n" 'suffix)))))

(define (iwd-shepherd-service config)
  (match-record config <iwd-configuration> (iwd openresolv coreutils)
    (let ((environment #~(list (string-append
                                "PATH=" (string-append #$openresolv "/sbin")
                                ":" (string-append #$coreutils "/bin")))))
      (list (shepherd-service
             (documentation "Run Iwd")
             (provision '(iwd networking))
             (requirement '(user-processes dbus-system loopback))
             (start #~(make-forkexec-constructor
                       (list (string-append #$iwd "/libexec/iwd"))
                       #:log-file "/var/log/iwd.log"
                       #:environment-variables #$environment))
             (stop #~(make-kill-destructor)))))))

(define %iwd-log-rotation
  (list (log-rotation (files '("/var/log/iwd.log")))))

(define (iwd-etc-service config)
  (match-record config <iwd-configuration> (config-file)
    `(("iwd/main.conf" ,config-file))))

(define iwd-service-type
  (let ((add-iwd-package (compose list iwd-configuration-iwd)))
    (service-type (name 'iwd)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            iwd-shepherd-service)
                         (service-extension dbus-root-service-type
                                            add-iwd-package)
                         (service-extension profile-service-type
                                            add-iwd-package)
                         (service-extension etc-service-type
                                            iwd-etc-service)
                         (service-extension rottlog-service-type
                                            (const %iwd-log-rotation))))
                  (default-value (iwd-configuration))
                  (description
                   "Run @url{https://iwd.wiki.kernel.org/,Iwd},
a network connection manager."))))
