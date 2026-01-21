(define-module (personal services utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)

  #:export (transform-shepherd-extension
            no-autostart
            online-service
            augment-computed-file
            chmod-computed-file
            info-package))

(define (no-autostart service)
  (shepherd-service (inherit service) (auto-start? #f)))

(define (online-service-transform service)
  (shepherd-service
    (inherit service)
    (requirement (cons 'online (shepherd-service-requirement service)))))

(define (transform-shepherd-extension input-service proc)
  "Augment shepherd extension of INPUT-SERVICE to disable auto-start."
  (define transform-extension
    (match-lambda
      (($ (@@ (gnu services) <service-extension>)
          (and ($ (@@ (gnu services) <service-type>)
                  (or 'shepherd-root 'home-shepherd) _) kind)
          compute)

       (service-extension
        kind
        (lambda (config)
          (let ((orig (car (compute config))))
            (list (proc orig))))))

      (ex ex)))

  (match input-service
    (($ (@@ (gnu services) <service>)
        (and ($ (@@ (gnu services) <service-type>) _ extensions _) kind)
        value)

     (service
      (service-type
        (inherit kind)
        (extensions (map transform-extension extensions)))
      value))))

(define (online-service type config)
  "Add 'online to the list of requirements of shepherd-service extension."
  (transform-shepherd-extension (service type config) online-service-transform))

(define (augment-computed-file proc content)
  "Wrap PROC returning a <computed-file> to append CONTENT to that file."
  (lambda* (#:rest r)
    (let ((f (apply proc r)))
      (computed-file
       (computed-file-name f)

       #~(begin
           #$(computed-file-gexp f)

           (let ((port (open-file #$output "a")))
             (format port #$content)
             (close port)))

       #:options (computed-file-options f)))))

(define (chmod-computed-file f p)
  (computed-file
   (computed-file-name f)
   #~(begin #$(computed-file-gexp f) (chmod #$output #$p))
   #:options (computed-file-options f)))

(define (info-package p)
  (let ((basename (package-name p)))
    (package
      (name (string-append basename "-info"))
      (version "0")
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let ((out (string-append #$output "/share/info")))
              (mkdir-p (dirname out))
              (symlink (string-append #$p "/share/info") out)))))
      (native-inputs (list p))
      (home-page (package-home-page p))
      (synopsis (string-append "Info pages of " basename))
      (description #f)
      (license (package-license p)))))
