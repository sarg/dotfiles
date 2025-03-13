(define-module (personal services hetzner)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (personal services utils)
  #:use-module (srfi srfi-1)

  #:export (hetzner-vm-service-type
            hetzner-vm))

(define-record-type* <hetzner-vm> hetzner-vm
  make-hetzner-vm
  hetzner-vm?
  this-hetzner-vm

  (name hetzner-vm-name)
  (type hetzner-vm-type)
  (image hetzner-vm-image)
  (cli-config hetzner-vm-cli-config)
  (user-data-file hetzner-vm-user-data-file (default #f))
  (extra-args hetzner-vm-extra-args (default '())))

(define (hetzner-vm-shepherd-service config)
  (match-record config <hetzner-vm> (name type image extra-args user-data-file cli-config)
    (list (shepherd-service
           (documentation "Creates hetzner VM.")
           (provision `(,(string->symbol (string-append "hetzner-" (hetzner-vm-name config)))))
           (requirement `(user-processes file-systems))
           (auto-start? #f)
           (start #~(lambda _ (invoke #$(file-append hetznercloud-cli "/bin/hcloud")
                                      "--config" #$cli-config
                                      "server" "create"
                                      "--name" #$name
                                      "--type" #$type
                                      "--image" #$image
                                      #$@(if user-data-file (list "--user-data-from-file" user-data-file) '())
                                      #$@extra-args)))

           (stop #~(lambda _ (invoke #$(file-append hetznercloud-cli "/bin/hcloud")
                                     "--config" #$cli-config
                                     "server" "delete" #$name)))))))

(define hetzner-vm-service-type
  (service-type
   (name 'hetzner-vm)
   (extensions
    (list (service-extension shepherd-root-service-type hetzner-vm-shepherd-service)))
   (compose concatenate)
   (description "Hetzner VM")))
