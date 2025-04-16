(use-modules (guix)
             (guix gexp)
             (guix git-download)
             (guix build-system copy)
             (guix build-system clojure)
             ((guix licenses) #:prefix license:)

             (personal packages binary)
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

(define clojure-tools
  (package
    (name "clojure-tools")
    (version "1.12.0.1517")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256 (base32 "1vsi7bina5q2rlw0jv92b0f208xpjcwjqzabdz0n0lvsdj3lws9q"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("deps.edn" "lib/clojure/")
         ("clojure-tools-1.12.0.1517.jar" "lib/clojure/libexec/")
         ("example-deps.edn" "lib/clojure/")
         ("tools.edn" "lib/clojure/")
         ("exec.jar" "lib/clojure/libexec/"))))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "CLI tools for the Clojure programming language")
    (description "The Clojure command line tools can be used to start a
Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
    (license license:epl1.0)))

(package
  (name "git2rss")
  (version "0.0.1-git")
  (source (local-file "." "git2rss-checkout"
                      #:recursive? #t
                      #:select? vcs-file?))

  (build-system copy-build-system)
  (inputs (list git babashka))
  (native-inputs (list clojure clojure-tools clojure-data-zip))
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
            (substitute* "git2rss.clj"
              (("(shell/sh )\"git\"" _ a)
               (string-append a "\"" (search-input-file inputs "/bin/git") "\"")))

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
            (chmod "git2rss" #o555))))))
  (home-page "https://github.com/sarg/dotfiles")
  (synopsis "Git2rss script")
  (description "Git2rss script")
  (license license:unlicense))
