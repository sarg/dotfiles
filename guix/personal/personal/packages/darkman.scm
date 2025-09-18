;; https://gitlab.com/podiki/guix-pod/-/blob/main/darkman.scm
(define-module (personal packages darkman)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages glib)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module (gnu packages man)
  #:use-module (gnu packages base)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public go-github-com-sj14-astral
  (package
    (name "go-github-com-sj14-astral")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sj14/astral")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1m4qirl3mrdpm1dw9lgfj6p7jsyy60kyhhzfkikxbf471wk5apba"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/sj14/astral/pkg/astral"
                 #:unpack-path "github.com/sj14/astral"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3
           go-github-com-pmezard-go-difflib
           go-github-com-davecgh-go-spew
           go-github-com-stretchr-testify
           go-github-com-logrusorgru-aurora-v3))
    (home-page "https://github.com/sj14/astral")
    (synopsis "Astral")
    (description "Calculations for the position of the sun and moon.")
    (license license:asl2.0)))

(define-public go-github-com-rxwycdh-rxhash
  (package
    (name "go-github-com-rxwycdh-rxhash")
    (version "0.0.0-20230131062142-10b7a38b400d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rxwycdh/rxhash")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qw4kn5r0xjfy9mycv57f7lmlpksybzr2qcdr4713svrxakwmgyz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rxwycdh/rxhash"))
    (home-page "https://github.com/rxwycdh/rxhash")
    (synopsis "rxhash")
    (description
     "rxhash is a Go library for creating a unique hash value for struct in Go, but
@@strong{data consistency}.")
    (license license:expat)))

(define-public darkman
  (package
    (name "darkman")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/WhyNotHugo/darkman.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "039xqi9pll7vl1m8lri5x626s7n0wqrjzyy979kh3wmpqbk8jz3j"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "gitlab.com/WhyNotHugo/darkman"
           ;; We don't need to install the source code for end-user applications.
           #:install-source? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key import-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" import-path "/contrib/dbus")
                     (substitute* '("org.freedesktop.impl.portal.desktop.darkman.service"
                                    "nl.whynothugo.darkman.service")
                       (("/usr/bin") (string-append #$output "/bin"))))))
               (replace 'build
                 (lambda* (#:key import-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" import-path)
                     (invoke "make" "build"))))
               (replace 'install
                 (lambda* (#:key import-path #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" import-path)
                     (invoke "make" "install" (string-append "PREFIX=" #$output))))))))
    (inputs (list go-github-com-adrg-xdg
                  go-github-com-godbus-dbus-v5
                  go-github-com-spf13-cobra
                  go-github-com-rxwycdh-rxhash
                  go-github-com-sj14-astral
                  go-github-com-kr-pretty
                  go-gopkg-in-check-v1
                  go-gopkg-in-yaml-v3))
    (native-inputs (list scdoc))
    (home-page
     "https://gitlab.com/WhyNotHugo/darkman")
    (synopsis "darkman")
    (description
     "This package provides a framework for dark-mode and light-mode transitions on Linux
desktop.")
    (license license:isc)))
