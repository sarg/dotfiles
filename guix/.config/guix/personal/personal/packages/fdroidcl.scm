(define-module (personal packages fdroidcl)
  #:use-module (gnu)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages check)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define-public fdroidcl
  (package
    (name "fdroidcl")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mvdan/fdroidcl/archive/"
                    "v" version ".tar.gz"))
              (sha256
               (base32 "0ybqxklhrp23xm89l4g6ms5qxm08db00kx4g6w9vyv11jd68qajk"))))
    (build-system go-build-system)

    (inputs
     `(("go-github-com-kr-pretty"
        ,go-github-com-kr-pretty)
       ;; ("go-github-com-rogpeppe-go-internal-testscript"
       ;;  ,go-github-com-rogpeppe-go-internal-testscript)
       ("go-gopkg.in-check.v1"
        ,go-gopkg.in-check.v1)))
    (arguments
     `(#:import-path "mvdan.cc/fdroidcl"

       #:phases
       (modify-phases %standard-phases
         (delete 'check))
       #:install-source? #f))
    (synopsis "fdroid cli")
    (description "fdroid cli")
    (home-page "https://github.com/mvdan/fdroidcl")
    (license bsd-3)))

(define-public go-github-com-rogpeppe-go-internal-testscript
  (package
    (name "go-github-com-rogpeppe-go-internal-testscript")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rogpeppe/go-internal/archive/"
                    "v" version ".tar.gz"))
              (sha256
               (base32 "0wm1720p99gbdy8flkkm6jrid9g8nm0ri2vc2pi35dbiv6js2pa1"))))
    (build-system go-build-system)

    (inputs
     `(("go-gopkg.in-check.v1"
        ,go-gopkg.in-check.v1)
       ("go-github-com-kr-pty"
        ,go-github-com-kr-pty)
       ("go-gopkg.in-errgo.v2-errors"
        ,go-gopkg.in-errgo.v2-errors)
       ("go-gopkg.in-errgo.v2-fmt"
        ,go-gopkg.in-errgo.v2-fmt)))
    (arguments
     `(#:import-path "github.com/rogpeppe/go-internal/testscript"
       #:unpack-path "github.com/rogpeppe/go-internal"
       #:install-source? #f))
    (synopsis "Selected Go-internal packages factored out from the standard library")
    (description "fdroid cli")
    (home-page "https://github.com/mvdan/fdroidcl")
    (license bsd-3)))

(define-public go-gopkg.in-errgo.v2-errors
  (package
    (name "go-gopkg.in-errgo.v2-errors")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/go-errgo/errgo/archive/"
                    "v" version ".tar.gz"))
              (sha256
               (base32 "0dxqq7slpgy5k8h40cy81vhy761vwjazdq2v89n5mjv5vdvhvcgn"))))
    (build-system go-build-system)
    (inputs
     `(("go-github-com-kr-pretty"
        ,go-github-com-kr-pretty)
       ("go-github-com-kr-text"
        ,go-github-com-kr-text)
       ("go-github-com-kr-pty"
        ,go-github-com-kr-pty)
       ("go-gopkg.in-check.v1"
        ,go-gopkg.in-check.v1)))
    (arguments
     '(#:import-path "gopkg.in/errgo.v2/errors"
       #:unpack-path "gopkg.in/errgo.v2"))
    (synopsis "")
    (description "")
    (home-page "https://gopkg.in/errgo.v2")
    (license bsd-3)))

(define-public go-gopkg.in-errgo.v2-fmt
  (package
    (name "go-gopkg.in-errgo.v2-fmt")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/go-errgo/errgo/archive/"
                    "v" version ".tar.gz"))
              (sha256
               (base32 "0dxqq7slpgy5k8h40cy81vhy761vwjazdq2v89n5mjv5vdvhvcgn"))))
    (build-system go-build-system)
    (inputs
     `(("go-github-com-kr-pretty"
        ,go-github-com-kr-pretty)
       ("go-github-com-kr-text"
        ,go-github-com-kr-text)
       ("go-github-com-kr-pty"
        ,go-github-com-kr-pty)
       ("go-gopkg.in-check.v1"
        ,go-gopkg.in-check.v1)))
    (arguments
     '(#:import-path "gopkg.in/errgo.v2/fmt/errors"
       #:unpack-path "gopkg.in/errgo.v2"))
    (synopsis "")
    (description "")
    (home-page "https://gopkg.in/errgo.v2")
    (license bsd-3)))

(define-public go-github-com-kr-pty
  (package
    (name "go-github-com-kr-pty")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kr/pty.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0383f0mb9kqjvncqrfpidsf8y6ns5zlrc91c6a74xpyxjwvzl2y6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kr/pty"))
    (synopsis "Text formatting in Go")
    (description "This package provides a text formatting functions in Go.")
    (home-page "https://github.com/kr/pty")
    (license expat)))
