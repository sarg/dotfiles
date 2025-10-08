(define-module (personal packages httptap)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-check)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix packages))

(define-public go-github-com-joemiller-certin
  (package
    (name "go-github-com-joemiller-certin")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joemiller/certin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kqivncmq3s2352nl4rxdkz5875f6my92zn363d2x5x76nasqfxw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/joemiller/certin"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-spf13-cobra))
    (home-page "https://github.com/joemiller/certin")
    (synopsis "TLS cert generator for test fixtures")
    (description
     "Certin is a Go library for quickly creating keys and certificates for
use as test fixtures.")
    (license license:expat)))

(define-public go-github-com-alexflint-go-scalar
  (package
    (name "go-github-com-alexflint-go-scalar")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexflint/go-scalar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c78yd7w5sxdfmnhqk8c4cyb2by0cp9qh3ab19sz5b5avbpwcary"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alexflint/go-scalar"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/alexflint/go-scalar")
    (synopsis "Scalar parsing library")
    (description
     "Scalar is a library for parsing strings into arbitrary scalars (integers,
floats, strings, booleans, etc).  It is helpful for tasks such as parsing
strings passed as environment variables or command line arguments.")
    (license license:bsd-2)))

(define-public go-github-com-alexflint-go-arg
  (package
    (name "go-github-com-alexflint-go-arg")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexflint/go-arg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1230v34rq7hbgk4sjkffxv3cmkqbmgbqvbwqad9gmgb5dg3rbfy4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:test-flags #~(list "-skip" "TestMixed|TestFloat")
      #:import-path "github.com/alexflint/go-arg"))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-github-com-alexflint-go-scalar))
    (home-page "https://github.com/alexflint/go-arg")
    (synopsis "Struct-based argument parsing in Go")
    (description
     "Arg parses command line arguments using the fields from a struct.")
    (license license:bsd-2)))

(define-public httptap
  (package
    (name "httptap")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/monasticacademy/httptap")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0746mhcmfy9pjiqn7p8hzxk8vb1bz6w1g8cbly4s9lrbmpm6wzha"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "experiments"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/monasticacademy/httptap"))
    (propagated-inputs (list go-gvisor-dev-gvisor
                             go-golang-org-x-sys
                             go-github-com-vishvananda-netns
                             go-github-com-vishvananda-netlink
                             go-github-com-songgao-water
                             go-github-com-miekg-dns
                             go-github-com-mdlayher-packet
                             go-github-com-alexflint-go-arg
                             go-software-sslmate-com-src-go-pkcs12
                             go-golang-org-x-tools
                             go-golang-org-x-lint
                             go-github-com-quic-go-quic-go
                             go-github-com-joemiller-certin
                             go-github-com-google-gopacket
                             go-github-com-gobwas-glob
                             go-github-com-fatih-color
                             go-github-com-ebitengine-purego))
    (home-page "https://github.com/monasticacademy/httptap")
    (synopsis "Intercept and display HTTP requests made by any linux program")
    (description
     "httptap isolates specified command in a network namespace and intercepts its
http(s) traffic.")
    (license license:expat)))
