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
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sj14/astral")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1rjr6d2rk2d7d8izd8v8rcx5vivfwqi5260y3g2spfq3f01jh8dg"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/sj14/astral"))
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

(define-public go-github-com-inconshreveable-mousetrap
  (package
    (name "go-github-com-inconshreveable-mousetrap")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/inconshreveable/mousetrap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1mn0kg48xkd74brf48qf5hzp0bc6g8cf5a77w895rl3qnlpfw152"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/inconshreveable/mousetrap"))
    (home-page "https://github.com/inconshreveable/mousetrap")
    (synopsis "mousetrap")
    (description "mousetrap is a tiny library that answers a single question.")
    (license license:asl2.0)))

(define-public go-github-com-integrii-flaggy
  (package
    (name "go-github-com-integrii-flaggy")
    (version "1.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/integrii/flaggy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qn55pn0c75bd4gm1fd2in0qp9fllfabwzn0qs994frd32cfz7h3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/integrii/flaggy"))
    (propagated-inputs `(("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/integrii/flaggy")
    (synopsis "Installation")
    (description
     "Package flaggy is a input flag parsing package that supports recursive
subcommands, positional values, and any-position flags without unnecessary
complexeties.")
    (license license:unlicense)))

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
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/WhyNotHugo/darkman.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ylk2zgn1bf65214ph0qrk0zv5hm689x4d3c0qwscgpl5xbjk88m"))))
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
                  go-github-com-integrii-flaggy
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
