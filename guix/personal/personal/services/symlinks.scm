(define-module (personal services symlinks)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (ice-9 match)

  #:export (resolve-relative-to
            home-symlinks-service-type))

(define symlinks-activation-script
  (program-file
   "install-symlinks"
   #~(begin
       (use-modules (ice-9 ftw))
       (define home-directory (getenv "HOME"))

       (define (install-symlinks source)
         (define (strip file)
           (string-drop file (string-length source)))

         (define (target-file n)
           (string-append home-directory "/" (strip n)))

         (define (enter? name stat result)
           (equal? 'directory (stat:type stat)))

         (define (leaf path stat result)
           (let ((target (target-file path)))
             (format #t "Installing ~a~%" target)
             (catch 'system-error
               (lambda () (symlink (readlink path) target))
               (const #t))))

         (define (down path stat result)
           (catch 'system-error
             (lambda () (mkdir (target-file path)))
             (const #t)))

         (file-system-fold enter? leaf down
                           (const #t)
                           (const #t)
                           (const #t)
                           #t source))

       (install-symlinks (string-append (getenv "GUIX_NEW_HOME") "/symlinks/")))))

(define (resolve-relative-to root links)
  "Resolves relative paths in the LINKS list of conses (source . target) relative to ROOT.
When target is not specified - use the same name as source. When target is a
directory (ends with a slash) - append source file name ot it. Example:
(resolve-relative-to \"/storage/\"
   '((\"data/gnupg\" . \".gnupg\") ; (\"/storage/data/gnupg\" . \".gnupg\")
     (\"Sync\") ; (\"/storage/Sync\" . \"Sync\")
     (\"data/gnupg/random_seed\" . \".gnupg/\") ; (\"/storage/data/gnupg/random_seed\" . \".gnupg/random_seed\")
     (\"/some/other/file\" . \"here\") ; as is"

  (map (match-lambda
         ((from)
          (cons (string-append root from)
                from))

         ((from . to)
          (cons (if (absolute-file-name? from)
                    from
                    (string-append root from))

                (if (string-suffix? "/" to)
                    (string-append to (basename from))
                    to))))
       links))

(define (symlinks-union symlinks)
  (computed-file
   "symlinks"
   (with-imported-modules (source-module-closure '((guix build utils)))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (for-each
          (match-lambda
            ((source . target)
             (let ((target-file (string-append #$output "/" target)))
               (mkdir-p (dirname target-file))
               (symlink source target-file))))
          
          '#$symlinks)))))

(define (symlinks-entry symlinks)
  (with-monad %store-monad
    (return `(("symlinks" ,(symlinks-union symlinks))))))

(define home-symlinks-service-type
  (service-type (name 'symlinks)
                (extensions
                 (list
                  (service-extension home-activation-service-type
                                     (const #~(primitive-load #$symlinks-activation-script)))
                  (service-extension home-service-type symlinks-entry)))
                (description
                 "symlink user-defined files without putting them into the store")))
