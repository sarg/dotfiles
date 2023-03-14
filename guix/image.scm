(use-modules (gnu system) (gnu system image) (gnu image))
(image
 (inherit efi-disk-image)
 (operating-system (operating-system-with-provenance (load "./system.scm")))
 (volatile-root? #false)
 (partition-table-type 'gpt))
