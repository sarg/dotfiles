;; NOTES: this image is WIP and requires patching guix
;; - https://git.sr.ht/~abcdw/rde/tree/044de83e980b7038b87d27a090aef24229df85eb/src/gnu/services/home.scm
(use-modules (gnu system) (gnu system image)
             (gnu image) (gnu home)
             (gnu services home))

(define (operating-system-with-homes os homes)
  (operating-system
    (inherit os)
    (packages
     (cons (@ (gnu packages backup) borg)
      (operating-system-packages os)))
    (services
     (cons
      (service guix-home-service-type
               (map (lambda (p)
                      (cons (car p)
                            (home-environment-with-provenance (cdr p))))
                    homes))

      (operating-system-user-services os)))))

(image
 (inherit efi-disk-image)
 (operating-system
   (operating-system-with-homes
    (operating-system-with-provenance (load "./system.scm"))
    `(("sarg" . ,(load "./home.scm")))))
 (volatile-root? #false))
