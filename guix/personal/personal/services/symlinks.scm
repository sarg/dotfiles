(define-module (personal services symlinks)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)

  #:export (resolve-relative-to))

(define (resolve-relative-to root links)
  "Takes a list of files suitable for home-files-service-type and expands entries
according to rules: relative paths resolved to ROOT, paths ending with slash
mean to use it as a directory."

  (define symlink
    (match-lambda
      ((to from)
       (list to (symlink-to from)))))

  (define complement
    (match-lambda
      ((from) (list from from))
      (x x)))

  (define expand-from
    (match-lambda
      ((and a (to from))
       (if (absolute-file-name? from)
           a
           (list to (string-append root from))))))

  (define (dir? a) (string-suffix? "/" a))

  (define resolve-from
    (match-lambda
      ((and a (to from))
       (if (dir? from)
           (map (lambda (f)
                  (list
                   (if (dir? to) to (string-append to "/"))
                   (string-append from f)))
            (scandir from
                     (negate (cut member <> '("." "..")))
                     string<?))

           (list a)))))

  (define resolve-to
    (match-lambda
      ((and a (to from))
       (if (dir? to)
           (list (string-append to (basename from)) from)
           a))))

  (map
   (compose symlink resolve-to)
   (append-map (compose resolve-from expand-from complement) links)))
