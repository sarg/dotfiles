(define-module (personal services symlinks)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)

  #:export (resolve-relative-to
            home-symlinks-service-type))

(define (symlinks-activation symlinks)
  "Takes a list of conses (from . to), where from are absolute file paths and to a relative to $HOME"
  #~(begin
      (define (no-follow-file-exists? file)
        "Return #t if file exists, even if it's a dangling symlink."
        (->bool (false-if-exception (lstat file))))

      (let ((home (string-append (getenv "HOME") "/")))
        (for-each
         (lambda (s)
           (let* ((source (car s))
                  (target (cdr s))
                  (target-file (if (absolute-file-name? target)
                                   target
                                   (string-append home target))))

             (unless (no-follow-file-exists? target-file)
               (symlink source target-file))))

         '#$symlinks))))

(define (resolve-relative-to root links)
  "Resolves relative paths in the LINKS list of conses (source . target) relative to ROOT.
When target is not specified - use the same name as source.
Example:
(resolve-relative-to \"/storage/\"
   '((\"data/gnupg\" . \".gnupg\") ; (\"/storage/data/gnupg\" . \".gnupg\")
     (\"Sync\") ; (\"/storage/Sync\" . \"Sync\")
     (\"/some/other/file\" . \"here\") ; as is"
  (map (match-lambda
         ((from)
          (cons (string-append root from)
                from))

         ((from . to)
          (cons (if (absolute-file-name? from) from (string-append root from))
                to)))
       links))

(define home-symlinks-service-type
  (service-type (name 'symlinks)
                (extensions (list (service-extension home-activation-service-type
                                                     symlinks-activation)))
                (description
                 "symlink user-defined files without putting them into the store")))
