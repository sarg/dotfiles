(define-module (personal packages scan)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages gnome))

(define-public simple-scan-with-airscan
  (package
    (name "simple-scan-with-airscan")
    (version (package-version simple-scan))
    (source #f) ; no source needed
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (guix build union))
      #:builder
      #~(begin
          (use-modules (ice-9 match)
                       (guix build utils)
                       (guix build union))

          (match %build-inputs
            (((names . directories) ...)
             (union-build %output directories
                          #:create-all-directories? #t)
             #t))

          (define (out n) (string-append %output n))
          (wrap-script (out "/bin/simple-scan")
            #:guile (string-append #$guile-3.0 "/bin/guile")
            `("LD_LIBRARY_PATH" = (,(out "/lib/sane")))
            `("SANE_CONFIG_DIR" = (,(out "/etc/sane.d"))))

          (substitute* (out "/share/applications/simple-scan.desktop")
            (((assoc-ref %build-inputs "simple-scan")) %output)))))

    (inputs (list simple-scan sane-airscan))
    (home-page #f)
    (synopsis "simple-scan wrapper which enables airscan function")
    (description "simple-scan with airscan function")
    (license #f)))
