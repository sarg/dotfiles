(define-module (personal services utils)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (ice-9 match)

  #:export (no-autostart))

(define (no-autostart input-service)
  (define (transform-extension ex)
    (match ex
      (($ (@@ (gnu services) <service-extension>)
          (and ($ (@@ (gnu services) <service-type>) 'shepherd-root _) kind)
          compute)

       (service-extension
        kind
        (lambda (config)
          (let ((orig (car (compute config))))
            (list (shepherd-service (inherit orig) (auto-start? #f)))))))

      (_ ex)))

  (match input-service
    (($ (@@ (gnu services) <service>)
        (and ($ (@@ (gnu services) <service-type>) _ extensions _) kind)
        value)

     (service
      (service-type
       (inherit kind)
       (extensions (map transform-extension extensions)))
      value))))
