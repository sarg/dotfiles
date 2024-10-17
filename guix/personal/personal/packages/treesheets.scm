(define-module (personal packages treesheets)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages))

(define-public treesheets
  (package
    (name "treesheets")
    (version "9723493654")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aardappel/treesheets")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0fkfbivaxq1l72i28cbnz1maffcj631mnbm8j3yqs3s3bk2cr4bk"))
              (modules '((guix build utils)))
              (snippet
               '(for-each delete-file-recursively
                 (list "lib" "osx" "treesheets")))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f #:cmake ,cmake))
    (native-inputs (list pkg-config))
    (inputs (list wxwidgets))
    (home-page "https://strlen.com/treesheets")
    (synopsis "Free form data organiser (hierarchical spreadsheet)")
    (description "A 'hierarchical spreadsheet' that is a great replacement for
 spreadsheets, mind mappers, outliners, PIMs, text editors and small databases.
Suitable for any kind of data organization, such as todo lists, calendars,
project management, brainstorming, organizing ideas, planning, requirements
gathering, presentation of information, etc.")
    (license license:zlib)))
