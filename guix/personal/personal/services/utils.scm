(define-module (personal services utils)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (ice-9 match)

  #:export (no-autostart augment-computed-file chmod-computed-file))

(define (no-autostart input-service)
  "Augment shepherd extension of INPUT-SERVICE to disable auto-start."
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
   #~(begin
       #$(computed-file-gexp f)
       (chmod #$output #$p))
   #:options (computed-file-options f)))
