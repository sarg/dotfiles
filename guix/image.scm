;; NOTE: this image is necessary only because efi-disk-image uses mbr partition table
;; otherwise just `guix system image --save-provenance --image-type=efi-raw --persistent system.scm`
(use-modules (gnu system) (gnu system image) (gnu image))
(image
 (inherit efi-disk-image)
 (operating-system (operating-system-with-provenance (load "./system.scm")))
 (volatile-root? #false)
 (partition-table-type 'gpt))
