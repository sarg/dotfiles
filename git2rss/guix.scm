(use-modules (guix)
             (guix gexp)
             (guix git-download)
             (guix build-system copy)
             (guix build-system clojure)
             ((guix licenses) #:prefix license:)

             (nongnu packages clojure)
             (gnu packages java)
             (gnu packages clojure)
             (gnu packages version-control))

(define vcs-file?
       ;; Return true if the given file is under version control.
       (or (git-predicate (current-source-directory))
           (const #t)))

(define-public clojure-data-zip
  (package
    (name "clojure-data-zip")
    (version "1.1.0")
    (home-page "https://github.com/clojure/data.zip")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1763ddrhflvi3y7mdrw7s5x3p1jnii25wz8vj0gw7i09cnpga5kn"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure")
       #:test-dirs '("src/test/clojure")
       #:doc-dirs '()))
    (synopsis "Clojure library for filtering trees, and XML trees in particular.")
    (description "@code{data.xml} is a Clojure library for filtering trees, and XML trees in particular.")
    (license license:epl1.0)))

(package
  (name "git2rss")
  (version "0.0.1-git")
  (source (local-file "." "git2rss-checkout"
                      #:recursive? #t
                      #:select? vcs-file?))

  (build-system copy-build-system)
  (inputs (list git babashka))
  (native-inputs (list clojure-data-zip))
  (arguments
   (list
    #:install-plan #~(list '("./git2rss" "bin/git2rss"))
    #:modules '((guix build copy-build-system)
                (guix build utils)
                (ice-9 textual-ports)
                (srfi srfi-1)
                (ice-9 match))
    #:phases
    #~(modify-phases %standard-phases
        (add-before 'install 'set-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "BABASHKA_CLASSPATH"
                    (string-join
                     (append-map (match-lambda
                                   ((label . dir)
                                    (find-files dir "\\.jar$")))
                                 inputs)
                     ":"))
            (invoke "bb" "uberscript" "git2rss" "git2rss.clj")
            (with-atomic-file-replacement "git2rss"
                                          (lambda (in out)
                                            (display "#!/usr/bin/env bb\n" out)
                                            (display (get-string-all in) out)))
            (chmod "git2rss" #o555)))
        (add-after 'patch-shebangs 'wrap-programs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (wrap-program (string-append out "/bin/git2rss")
                `("BABASHKA_CLASSPATH" ":" = (,(getenv "BABASHKA_CLASSPATH")))
                `("PATH" ":" = (,(string-append (assoc-ref inputs "git") "/bin"))))))))))
  (home-page "https://github.com/sarg/dotfiles")
  (synopsis "Git2rss script")
  (description "Git2rss script")
  (license license:unlicense))
