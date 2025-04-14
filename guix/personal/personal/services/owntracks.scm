(define-module (personal services owntracks)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (personal packages owntracks)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages messaging)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (owntracks-service-type
            owntracks-configuration
            owntracks-service))

;; owntracks-configuration
(define-maybe string (prefix owntracks-))

(define-maybe number (prefix owntracks-))

(define (uglify-field-name name)
  (string-append
   "OTR_"
   (match name
     ('clean-age "CLEAN_AGE")
     (_ (let ((str (symbol->string name)))
          (string-upcase (string-delete #\- str)))))))

(define (owntracks-serialize-field field-name value)
  #~(format #f "~a=~a\n" #$(uglify-field-name field-name) #$value))

(define owntracks-serialize-string owntracks-serialize-field)

(define (owntracks-serialize-number field-name value)
  (owntracks-serialize-field field-name (number->string value)))

(define (owntracks-serialize-list-of-strings field-name value)
  (owntracks-serialize-field field-name (string-join value " ")))

(define-configuration owntracks-configuration
  (storage-dir
   (string "/var/lib/owntracks")
   "Pathname to the storage directory")
  (mqtt?
   (boolean #t)
   "Enable MQTT."
   empty-serializer)                    ; TODO set port to 0
  (host
   (string "localhost")
   "MQTT hostname/address to connect to")
  (port
   (number 1883)
   "MQTT port number to connect to.")
  (user
   maybe-string
   "MQTT username")
  (pass
   maybe-string
   "MQTT password")
  (qos
   (number 2)
   "MQTT QoS")
  (client-id
   maybe-string
   "MQTT ClientID")
  (http-host
   (string "localhost")
   "Address for the HTTP module to bind to")
  (http-port
   (number 8083)
   "Port number of the HTTP module to bind to")
  (http-prefix
   maybe-string
   "Prefix of URL of this Recorder (e.g. `https://example.com/recorder/`")
  (http-logdir
   maybe-string
   "Directory in which to store access.log. Override with --http-logdir")
  (lua-script
   maybe-string
   "Path to the Lua script")
  (precision
   (number 7)
   "Reverse-geo precision")
  (geokey
   maybe-string
   "API key for reverse-geo lookups")
  (browser-apikey
   maybe-string
   "Google maps browser API key")
  (topics
   (list-of-strings '("owntracks/+/+"))
   "List of topics to subscribe to for MQTT")
  (cafile
   maybe-string
   "Path to PEM-encoded CA certificate file for MQTT (implicitly enables TLS)")
  (capath
   maybe-string
   "Directory of c_rehashed PEM certificates")
  (certfile
   maybe-string
   "Path to PEM-encoded client certificate for MQTT")
  (keyfile
   maybe-string
   "Path to PEM-encoded client key for MQTT")
  (identity
   maybe-string
   "MQTT identity for PSK")
  (psk
   maybe-string
   "MQTT PSK")
  (server-label
   (string "OwnTracks")
   "server label for Web")
  (lmdb-size
   (number 5368709120)
   "size of the LMDB database (5GB). If less than 10485760 (10 MB) it will be set to 10485760.")
  (clean-age
   (number 0)
   "purge geo gcache entries after these seconds")
  (prefix owntracks-))

;; owntracks-service-type
(define-record-type* <owntracks-service>
  owntracks-service make-owntracks-service
  owntracks-service?
  (owntracks owntracks-service-owntracks
             (default owntracks-recorder))
  (log-file  owntracks-service-log-file       ;string | gexp
             (default #~(string-append %user-log-dir "/owntracks.log")))
  (config owntracks-service-config
          (default (owntracks-configuration))))

(define (owntracks-config-file config)
  (mixed-text-file "owntracks.conf"
    (serialize-configuration
      config
      owntracks-configuration-fields)))

(define owntracks-shepherd-service
  (match-lambda
    (($ <owntracks-service> owntracks log-file (= owntracks-config-file config))
     (list (shepherd-service
            (documentation "Runs owntracks instance.")
            (provision '(owntracks))
            (modules `((ice-9 rdelim)
                       ((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                       ,@%default-modules))
            (actions (list (shepherd-configuration-action config)))
            (start #~(make-forkexec-constructor
                      (list (string-append #$owntracks "/bin/ot-recorder"))
                      #:log-file #$log-file
                      #:environment-variables
                      (call-with-input-file #$config
                        (lambda (port)
                          (let loop ((line (read-line port))
                                     (lines '()))
                            (if (eof-object? line)
                                (reverse lines)
                                (loop (read-line port)
                                      (cons line lines))))))))
            (stop #~(make-kill-destructor)))))))

(define-public owntracks-service-type
  (service-type
   (name 'owntracks)
   (extensions
    (list (service-extension home-shepherd-service-type
                             owntracks-shepherd-service)))
   (compose concatenate)
   (default-value (owntracks-service))
   (description "Owntracks service.")))

;; mosquitto-service-type
(define (mosquitto-shepherd-service config)
  "Return a <shepherd-service> for mosquitto with CONFIG."

  (list (shepherd-service
         (documentation "mosquitto mqtt server")
         (provision '(mosquitto mqtt))
         (modules `(((shepherd support) #:hide (mkdir-p)) ;for '%user-log-dir'
                    ,@%default-modules))
         (actions (list (shepherd-configuration-action config)))
         (start #~(make-forkexec-constructor
                   (list (string-append #$mosquitto "/sbin/mosquitto")
                         "-c" #$config)
                   #:log-file (string-append %user-log-dir "/mosquitto.log")))
         (stop #~(make-kill-destructor)))))

(define-public mosquitto-service-type
  (service-type
   (name 'mosquitto)
   (description
    "run mosquitto server")
   (extensions
    (list (service-extension home-shepherd-service-type
                             mosquitto-shepherd-service)))))
