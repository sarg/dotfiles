(define-module (personal utils)
  #:use-module (gnu)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)

  #:export (as-local-files))

(define (del-prefix p str)
  (if (string-prefix? p str)
      (substring/shared str (string-length p))
      str))

;; todo: simplify
(define* (as-local-files dir #:optional (trim-prefix dir))
  (let ((absolute-dir (string-append (getcwd) "/" dir))
        (to-trim (string-append (getcwd) "/" trim-prefix "/")))
    (map (lambda (fn)
           (list
            (del-prefix to-trim fn)
            (local-file (canonicalize-path fn) (del-prefix "." (basename fn)) #:recursive? #t)))
         (find-files absolute-dir))))
