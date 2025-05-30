(define-module (personal packages jaro)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages web))

;; TODO: https://issues.guix.gnu.org/72030
(define-public perl-file-mimeinfo-patched
  (package
    (inherit perl-file-mimeinfo)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs perl-file-mimeinfo)
       (prepend perl-encode-locale)))))

(define-public jaro
  (let ((commit "a5744a686e43a148536b04db5be779aabfed1603")
        (revision "1"))
    (package
      (name "jaro")
      (version (git-version "0.5.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/isamert/jaro")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0jf6ciwvk0ix9gv1gymmyhicx07hsxivryf6fcqvi7gg38czqwzl"))))
      (build-system copy-build-system)
      (native-inputs (list guile-3.0))
      (inputs (list perl-file-mimeinfo-patched))
      (arguments
       (list
        #:install-plan #~(list '("./jaro" "bin/xdg-open"))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'install 'set-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "jaro"
                  (("^exec guile")
                   (string-append "exec " (search-input-file inputs "/bin/guile")))
                  (("\"mimetype\"")
                   (string-append "\"" (search-input-file inputs "/bin/mimetype") "\""))))))))
      (home-page "https://github.com/isamert/jaro")
      (synopsis "Resource opener implemented in scheme")
      (description "Jaro is a xdg-open replacement implemented in Scheme.")
      (license license:gpl3+))))
