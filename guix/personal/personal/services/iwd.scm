;; from https://issues.guix.gnu.org/59971
(define-module (personal services iwd)
  #:export (iwd-configuration
            iwd-configuration-config
            iwd-configuration-ignored-interfaces
            iwd-configuration-ignored-phys
            iwd-configuration-interfaces
            iwd-configuration-iwd
            iwd-configuration-phys
            iwd-configuration-shepherd-provision
            iwd-configuration-shepherd-requirement
            iwd-service-type
            iwd-general-settings
            iwd-network-settings
            iwd-scan-settings
            iwd-settings)

  #:use-module (ice-9 curried-definitions)
  #:use-module (gnu)
  #:use-module (gnu home services utils)
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

(define-syntax-rule (id ctx parts ...)
  "Assemble PARTS into a raw (unhygienic) identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

(define-syntax define-enumerated-field-type
  (lambda (x)
    (syntax-case x (prefix)
      ((_ name (option ...) (prefix serializer-prefix))
       #`(begin
           (define (#,(id #'name #'name #'?) x)
             (memq x '(option ...)))
           (define (#,(id #'name #'serializer-prefix #'serialize- #'name) field-name val)
             (#,(id #'name #'serializer-prefix #'serialize-field) field-name val))))

      ((_ name (option ...))
       #`(define-enumerated-field-type name (option ...) (prefix #{}#))))))

(define (iwd-uglify-field-name name)
  (object->camel-case-string name 'upper))

(define (iwd-serialize-base field-name val)
  (format #f "~a=~a\n" field-name val))

(define (iwd-serialize-field field-name val)
  (iwd-serialize-base (iwd-uglify-field-name field-name) val))

(define iwd-serialize-number iwd-serialize-field)

(define (iwd-serialize-boolean field-name value)
  (iwd-serialize-field
   (string-trim-right (symbol->string field-name) #\?)
   (if value "true" "false")))

(define (iwd-serialize-alist field-name value)
  (if (null? value)
      ""
      #~(string-append #$@(generic-serialize-alist list
                                                   iwd-serialize-base
                                                   value))))

(define-enumerated-field-type resolving-service
  (none systemd resolvconf)
  (prefix iwd-))

(define-configuration iwd-general-settings
  (enable-network-configuration?
   (boolean #f)
   "Setting this option to true enables @code{iwd} to configure the network
interfaces with the IP addresses.")
  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values to be
appended to the General settings group.")

  (prefix iwd-))

(define-configuration iwd-network-settings
  (enable-ipv6?
   (boolean #t)
   "Sets the global default that tells @code{iwd} whether it should configure
IPv6 addresses and routes")

  (name-resolving-service
   (resolving-service 'none)
   "Configures a DNS resolution method used by the system.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values to be
appended to the Network settings group.")

  (prefix iwd-))

(define-configuration iwd-scan-settings
  (disable-periodic-scan?
   (boolean #f)
   "Setting this option to @code{#t} will prevent @code{iwd} from issuing the
periodic scans for the available networks while disconnected.")

  (initial-periodic-scan-interval
   (number 10)
   "The initial periodic scan interval upon disconnect (in seconds).")

  (maximum-periodic-scan-interval
   (number 300)
   "The maximum periodic scan interval (in seconds).")

  (disable-roaming-scan?
   (boolean #f)
   "Setting this option to @code{#t} will prevent @code{iwd} from trying to scan
when roaming decisions are activated.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values to be
appended to the Scan settings group.")

  (prefix iwd-))

(define-maybe iwd-network-settings)
(define-maybe iwd-scan-settings)

(define ((iwd-serialize-config-section fields) name cfg)
  #~(format #f "[~a]\n~a\n"
            #$(string-upcase (object->string name) 0 1)
            #$(serialize-configuration cfg fields)))

(define serialize-iwd-network-settings
  (iwd-serialize-config-section iwd-network-settings-fields))

(define serialize-iwd-scan-settings
  (iwd-serialize-config-section iwd-scan-settings-fields))

(define serialize-iwd-general-settings
  (iwd-serialize-config-section iwd-general-settings-fields))

(define-configuration iwd-settings
  (general
   (iwd-general-settings (iwd-general-settings))
   "General settings.")

  (network
   maybe-iwd-network-settings
   "Network settings.")

  (scan
   maybe-iwd-scan-settings
   "Scan settings.")

  (extra-config
   (list-of-strings '())
   "Extra configuration values to append to the IWD configuration file."
   (serializer (lambda (_ value) (string-join value "\n" 'suffix)))))

(define-configuration/no-serialization iwd-configuration
  (iwd
   (file-like iwd)
   "The IWD package to use.")

  (interfaces
   (list-of-strings '())
   "If this is set, it must specify @dfn{glob patterns} matching network
interfaces that IWD will control.")

  (ignored-interfaces
   (list-of-strings '())
   "If this is set, it must specify @dfn{glob patterns} matching network
interfaces that IWD will not manage.")

  (phys
   (list-of-strings '())
   "If this is set, it must specify @dfn{glob patterns} matching network
PHYs names that IWD will control.")

  (ignored-phys
   (list-of-strings '())
   "If this is set, it must specify @dfn{glob patterns} matching network
PHYs names that IWD will not manage.")

  (shepherd-requirement
   (list-of-symbols '())
   "Shepherd requirements the service should depend on.")

  (shepherd-provision
   (list-of-symbols '(iwd wireless-daemon))
   "The name(s) of the service.")

  (config
   (iwd-settings (iwd-settings))
   "Configuration settings."))

(define (iwd-generate-documentation)
  (configuration->documentation 'iwd-configuration)
  (configuration->documentation 'iwd-settings)
  (configuration->documentation 'iwd-general-settings)
  (configuration->documentation 'iwd-network-settings)
  (configuration->documentation 'iwd-scan-settings))

(define (iwd-config-file config)
  "Return an IWD configuration file."
  (mixed-text-file "main.conf"
                   (serialize-configuration
                    (iwd-configuration-config config)
                    iwd-settings-fields)))

(define (iwd-environment config)
  (let ((resolver
         (and=> (iwd-settings-network (iwd-configuration-config config))
                iwd-network-settings-name-resolving-service)))

    (if (eq? resolver 'resolvconf)
        #~(list (string-append "PATH=" #$openresolv "/sbin"))
        '())))

(define (iwd-shepherd-service config)
  (match-record config <iwd-configuration>
                (iwd interfaces ignored-interfaces
                     phys ignored-phys
                     shepherd-requirement shepherd-provision)

    (list (shepherd-service
           (documentation "Run Internet Wireless Daemon")
           (provision shepherd-provision)
           (requirement `(user-processes dbus-system loopback ,@shepherd-requirement))
           (start #~(make-forkexec-constructor
                     (list (string-append #$iwd "/libexec/iwd")
                           "--logger=syslog"
                           #$@(if (null? interfaces) '()
                                  (list (string-append "--interfaces="
                                                       (string-join interfaces ","))))
                           #$@(if (null? ignored-interfaces) '()
                                  (list (string-append "--nointerfaces="
                                                       (string-join ignored-interfaces ","))))
                           #$@(if (null? phys) '()
                                  (list (string-append "--phys="
                                                       (string-join phys ","))))
                           #$@(if (null? ignored-phys) '()
                                  (list (string-append "--nophys="
                                                       (string-join ignored-phys ",")))))

                     #:environment-variables
                     #$(iwd-environment config)))
           (stop #~(make-kill-destructor))))))

(define (iwd-etc-service config)
  `(("iwd/main.conf" ,(iwd-config-file config))))

(define iwd-service-type
  (let ((add-iwd-package (compose list iwd-configuration-iwd)))
    (service-type (name 'iwd)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            iwd-shepherd-service)
                         (service-extension etc-service-type
                                            iwd-etc-service)
                         (service-extension dbus-root-service-type
                                            add-iwd-package)
                         (service-extension profile-service-type
                                            add-iwd-package)))
                  (default-value (iwd-configuration))
                  (description
                   "Run @url{https://iwd.wiki.kernel.org/,Iwd},
a network connection manager."))))
