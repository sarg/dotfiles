(define-module (personal packages mumi)
  #:use-module (personal packages jaro)
  #:use-module (guix utils)
  #:use-module (gnu packages mail)
  #:use-module (guix packages))

(define-public mumi-with-jaro
  (package
    (inherit mumi)
    (name "mumi-with-jaro")
    (inputs
     (modify-inputs (package-inputs mumi)
       (replace "xdg-utils" jaro)))))
