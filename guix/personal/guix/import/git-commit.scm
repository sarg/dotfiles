;;; GNU Guix --- Functional package management for GNU

(define-module (guix import git-commit)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix import utils) #:select (find-version))
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:export (%generic-git-commit-updater))

(define* (import-git-commit package #:key version partial-version?)
  "Return an <upstream-source> for the latest commit of PACKAGE.
Optionally include a VERSION string to fetch a specific version."
  (let* ((name (package-name package))
         (old-version (package-version package))
         (props (package-properties package))
         (old-reference (origin-uri (package-source package)))
         (url (git-reference-url old-reference))
         (commit (or version (receive (dir sha1 rel) (update-cached-checkout url) sha1))))
    (and commit
         (upstream-source
          (package name)
          (version
           (git-version
            (first (string-split old-version #\-))
            (or (assoc-ref props 'revision) "0")
            commit))
          (urls (git-reference
                 (url url)
                 (commit commit)
                 (recursive? (git-reference-recursive? old-reference))))))))

(define (git-package? package)
  "Return true if PACKAGE is hosted on a Git repository."
  (match (package-source package)
    ((? origin? origin)
     (and (eq? (origin-method origin) git-fetch)
          (git-reference? (origin-uri origin))))
    (_ #f)))

(define %generic-git-commit-updater
  (upstream-updater
   (name 'git-commit)
   (description "Updater for packages hosted on Git repositories")
   (pred git-package?)
   (import import-git-commit)))
