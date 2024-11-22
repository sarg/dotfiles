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

(define-public go-github-com-adrg-xdg
  (package
    (name "go-github-com-adrg-xdg")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/adrg/xdg")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1h11myyy426qrz96h9jrl7r1n8xwd5ybcr9c4s8l2pxn86dn5jsy"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #false ; fails trying to create directories
       #:import-path "github.com/adrg/xdg"))
    (propagated-inputs
      `(("go-github-com-stretchr-testify"
         ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/adrg/xdg")
    (synopsis "Installation")
    (description
      "Package xdg provides an implementation of the XDG Base Directory
Specification.  The specification defines a set of standard paths for storing
application files including data and configuration files.  For portability and
flexibility reasons, applications should use the XDG defined locations instead
of hardcoding paths.  The package also includes the locations of well known user
directories.")
    (license license:expat)))

(define-public go-github-com-godbus-dbus-v5
  (package
    (name "go-github-com-godbus-dbus-v5")
    (version "5.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/godbus/dbus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Disable tests which require a system D-Bus instance.
           (substitute* "conn_test.go"
             (("func TestSystemBus")
              "func DisabledTestSystemBus")
             (("func TestConnectSystemBus")
              "func DisabledTestConnectSystemBus"))))
       (sha256
        (base32
         "0d7740bjprd6mhs1wmhd53fb3kf61dz0hp1a0dda3dc28w2z84pp"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/godbus/dbus"
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (when tests?
               (invoke "dbus-run-session" "--" "go" "test" import-path)))))))
    (native-inputs
     (list dbus)) ;dbus-launch
    (home-page "https://github.com/godbus/dbus/")
    (synopsis "Native Go client bindings for the D-Bus")
    (description "@code{dbus} is a library that implements native Go client
bindings for the D-Bus message bus system.")
    (license license:bsd-2)))

(define-public go-github-com-logrusorgru-aurora-v3
  (package
    (name "go-github-com-logrusorgru-aurora-v3")
    (version "3.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/logrusorgru/aurora")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0z7cgj8gl69271d0ag4f4yjbsvbrnfibc96cs01spqf5krv2rzjc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/logrusorgru/aurora/v3"))
    (home-page "https://github.com/logrusorgru/aurora")
    (synopsis "Aurora")
    (description "Package aurora implements ANSI-colors")
    (license license:unlicense)))

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

(define-public go-github-com-russross-blackfriday-v2
  (package
    (name "go-github-com-russross-blackfriday-v2")
    (version "2.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/russross/blackfriday")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0d1rg1drrfmabilqjjayklsz5d0n3hkf979sr3wsrw92bfbkivs7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/russross/blackfriday/v2"))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Blackfriday")
    (description "Package blackfriday is a markdown processor.")
    (license license:bsd-2)))

(define-public go-github-com-cpuguy83-go-md2man-v2
  (package
    (name "go-github-com-cpuguy83-go-md2man-v2")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cpuguy83/go-md2man")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "051ljpzf1f5nh631lvn53ziclkzmx5lza8545mkk6wxdfnfdcx8f"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cpuguy83/go-md2man/v2"))
    (propagated-inputs
      `(("go-github-com-russross-blackfriday-v2"
         ,go-github-com-russross-blackfriday-v2)))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "go-md2man")
    (description "Converts markdown into roff (man pages).")
    (license license:expat)))

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

(define-public darkman
  (package
    (name "darkman")
    (version "1.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/WhyNotHugo/darkman.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09yrvijdii3m9dajp12ax3zgisy39lffhkkzbhf5qzc4xramf8z9"))))
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
