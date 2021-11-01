(define-module (personal packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages time)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public python-markdown2
  (package
    (name "python-markdown2")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "markdown2" version))
        (sha256
          (base32
            "06wvhai41in8xdvwmn97d6d0fyvlhsqyj0agd27zdrj4wpq6kmr8"))))
    (build-system python-build-system)
    (home-page
      "https://github.com/trentm/python-markdown2")
    (synopsis
      "A fast and complete Python implementation of Markdown")
    (description
      "A fast and complete Python implementation of Markdown")
    (license license:expat)))

(define-public python-pytest-freezegun
  (package
    (name "python-pytest-freezegun")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-freezegun" version ".zip"))
        (sha256
          (base32
            "0jb92x165z9nckgz11flgdwc3apzbkxq396ajbng66vm6db2vj0r"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-freezegun" ,python-freezegun)
        ("python-pytest" ,python-pytest)))
    (native-inputs `(("unzip" ,unzip)))
    (home-page
      "https://github.com/ktosiek/pytest-freezegun")
    (synopsis
      "Wrap tests with fixtures in freeze_time")
    (description
      "Wrap tests with fixtures in freeze_time")
    (license license:expat)))

(define-public python-pytest-black-multipy
  (package
    (name "python-pytest-black-multipy")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-black-multipy" version))
        (sha256
          (base32
            "1bis1byh9bpd3695yy660zmid82cq281xxbinpipz68nbjzggqz1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-pytest-black" ,python-pytest-black)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page
      "https://github.com/jaraco/pytest-black-multipy")
    (synopsis "Allow '--black' on older Pythons")
    (description "Allow '--black' on older Pythons")
    (license #f)))

(define-public python-tempora
  (package
    (name "python-tempora")
    (version "4.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tempora" version))
        (sha256
          (base32
            "02crszfnph5v2jj4idi513y84f89dnxjb8ihl8wxz82zz2dw5z8h"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-pytz" ,python-pytz)))
    (native-inputs
      `(("python-freezegun" ,python-freezegun)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-freezegun"
         ,python-pytest-freezegun)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page "https://github.com/jaraco/tempora")
    (synopsis
      "Objects and routines pertaining to date and time (tempora)")
    (description
      "Objects and routines pertaining to date and time (tempora)")
    (license #f)))

(define-public python-portend
  (package
    (name "python-portend")
    (version "2.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "portend" version))
        (sha256
          (base32
            "0as82rs7mhqlkj6j74lsj7lxrssvq9hixhplnnsqfjp6g2idjvlq"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-tempora" ,python-tempora)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page "https://github.com/jaraco/portend")
    (synopsis "TCP port monitoring and discovery")
    (description "TCP port monitoring and discovery")
    (license #f)))

(define-public python-jaraco.text
  (package
    (name "python-jaraco.text")
    (version "3.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.text" version))
        (sha256
          (base32
            "1cc98gm09sqzxik80n1kr28ih6y71nd8p50mp67aj5sah5v10zzh"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-importlib-resources"
         ,python-importlib-resources)
        ("python-jaraco.functools"
         ,python-jaraco.functools)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page
      "https://github.com/jaraco/jaraco.text")
    (synopsis "Module for text manipulation")
    (description "Module for text manipulation")
    (license #f)))

(define-public python-pytest-watch
  (package
    (name "python-pytest-watch")
    (version "4.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-watch" version))
        (sha256
          (base32
            "1fflnd3varpqy8yzcs451n8h7wmjyx1408qdin5p2qdksl1ny4q6"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-colorama" ,python-colorama)
        ("python-docopt" ,python-docopt)
        ("python-pytest" ,python-pytest)
        ("python-watchdog" ,python-watchdog)))
    (home-page
      "http://github.com/joeyespo/pytest-watch")
    (synopsis
      "Local continuous test runner with pytest and watchdog.")
    (description
      "Local continuous test runner with pytest and watchdog.")
    (license license:expat)))

(define-public python-jaraco.classes
  (package
    (name "python-jaraco.classes")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.classes" version))
        (sha256
          (base32
            "0d6g7qvfv1jlzbzh6asprqdblqd59grvlvr3nwbdqdqrmwlbfm7d"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page
      "https://github.com/jaraco/jaraco.classes")
    (synopsis
      "Utility functions for Python class constructs")
    (description
      "Utility functions for Python class constructs")
    (license #f)))

(define-public python-pytest-mypy
  (package
    (name "python-pytest-mypy")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-mypy" version))
        (sha256
          (base32
            "049v7y4zv2l0ymj03casr5fad8hm89lvvhx1rd7ha7dzlhimg98z"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-attrs" ,python-attrs)
        ("python-filelock" ,python-filelock)
        ("python-mypy" ,python-mypy)
        ("python-pytest" ,python-pytest)))
    (home-page
      "https://github.com/dbader/pytest-mypy")
    (synopsis
      "Mypy static type checker plugin for Pytest")
    (description
      "Mypy static type checker plugin for Pytest")
    (license license:expat)))

(define-public python-jaraco.context
  (package
    (name "python-jaraco.context")
    (version "4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.context" version))
        (sha256
          (base32
            "11gmkfk67hgv389nf77rrrk1j6bjk5v52ndpdy92x56y3qd4mpsc"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page
      "https://github.com/jaraco/jaraco.context")
    (synopsis "Context managers by jaraco")
    (description "Context managers by jaraco")
    (license #f)))

(define-public python-pytest-enabler
  (package
    (name "python-pytest-enabler")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-enabler" version))
        (sha256
          (base32
            "185vsg88sy10h9b2zfsi5ivdmx79salrwrp2c2i7s6p3n463jxji"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.context" ,python-jaraco.context)
        ("python-jaraco.functools"
         ,python-jaraco.functools)
        ("python-more-itertools" ,python-more-itertools)
        ("python-toml" ,python-toml)))
    (native-inputs
      `(("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page
      "https://github.com/jaraco/pytest-enabler")
    (synopsis "Enable installed pytest plugins")
    (description "Enable installed pytest plugins")
    (license #f)))

(define-public python-jaraco.functools
  (package
    (name "python-jaraco.functools")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jaraco.functools" version))
        (sha256
          (base32
            "055k8s04fgj1lkw30ynddpvdrhd6vdclnx5hi40rh3ia3skpvkxz"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-more-itertools" ,python-more-itertools)))
    (native-inputs
      `(("python-jaraco.classes" ,python-jaraco.classes)
        ("python-pytest" ,python-pytest)
        ("python-pytest-black" ,python-pytest-black)
        ("python-pytest-checkdocs"
         ,python-pytest-checkdocs)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-enabler" ,python-pytest-enabler)
        ("python-pytest-flake8" ,python-pytest-flake8)
        ("python-pytest-mypy" ,python-pytest-mypy)))
    (home-page
      "https://github.com/jaraco/jaraco.functools")
    (synopsis "Functools like those found in stdlib")
    (description
      "Functools like those found in stdlib")
    (license #f)))

(define-public python-cheroot
  (package
    (name "python-cheroot")
    (version "8.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cheroot" version))
        (sha256
          (base32
            "0rqmq3xcv08fz3cfv33ckwimqrk8h74sfmx5prj16nqmslzx0dzi"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-jaraco.functools" ,python-jaraco.functools)
        ("python-more-itertools" ,python-more-itertools)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-codecov" ,python-codecov)
        ("python-colorama" ,python-colorama)
        ("python-coverage" ,python-coverage)
        ("python-jaraco.context" ,python-jaraco.context)
        ("python-jaraco.text" ,python-jaraco.text)
        ("python-portend" ,python-portend)
        ("python-pyopenssl" ,python-pyopenssl)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-pytest-forked" ,python-pytest-forked)
        ("python-pytest-mock" ,python-pytest-mock)
        ("python-pytest-sugar" ,python-pytest-sugar)
        ("python-pytest-watch" ,python-pytest-watch)
        ("python-pytest-xdist" ,python-pytest-xdist)
        ("python-requests-toolbelt"
         ,python-requests-toolbelt)
        ("python-requests-unixsocket"
         ,python-requests-unixsocket)
        ("python-trustme" ,python-trustme)
        ("python-urllib3" ,python-urllib3)
        ("python-watchdog" ,python-watchdog)))
    (home-page "https://cheroot.cherrypy.org")
    (synopsis
      "Highly-optimized, pure-python HTTP server")
    (description
      "Highly-optimized, pure-python HTTP server")
    (license #f)))

(define-public python-googleapis-common-protos
  (package
    (name "python-googleapis-common-protos")
    (version "1.53.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "googleapis-common-protos" version))
        (sha256
          (base32
            "1x7bahcgnj4hnjb096s30ryad2iw5pv5qbgc7in1za507a8fi3m8"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-protobuf" ,python-protobuf)))
    (home-page
      "https://github.com/googleapis/python-api-common-protos")
    (synopsis "Common protobufs used in Google APIs")
    (description
      "Common protobufs used in Google APIs")
    (license #f)))

(define-public python-google-api-core
  (package
    (name "python-google-api-core")
    (version "1.30.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-core" version))
        (sha256
          (base32
            "07210db95dpnvpibin8b1whwa4vqh02yxpqhpiixgcwlsdad6907"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-auth" ,python-google-auth)
        ("python-googleapis-common-protos"
         ,python-googleapis-common-protos)
        ("python-packaging" ,python-packaging)
        ("python-protobuf" ,python-protobuf)
        ("python-pytz" ,python-pytz)
        ("python-requests" ,python-requests)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/googleapis/python-api-core")
    (synopsis "Google API client core library")
    (description "Google API client core library")
    (license license:asl2.0)))

(define-public python-google-auth-httplib2
  (package
    (name "python-google-auth-httplib2")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth-httplib2" version))
        (sha256
          (base32
            "1b1hrhah01hx6bj3rb83iybrdwqv0bbdy63py39srv1bcgykjz50"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-auth" ,python-google-auth)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/GoogleCloudPlatform/google-auth-library-python-httplib2")
    (synopsis
      "Google Authentication Library: httplib2 transport")
    (description
      "Google Authentication Library: httplib2 transport")
    (license license:asl2.0)))

(define-public python-google-auth
  (package
    (name "python-google-auth")
    (version "1.31.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-auth" version))
        (sha256
          (base32
            "0pnn9yfafswxk1fmgv04k85bnkdxmw9dnspk4vvacyfnqn4phkqm"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-cachetools" ,python-cachetools)
        ("python-pyasn1-modules" ,python-pyasn1-modules)
        ("python-rsa" ,python-rsa)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/googleapis/google-auth-library-python")
    (synopsis "Google Authentication Library")
    (description "Google Authentication Library")
    (license license:asl2.0)))

(define-public python-google-api-python-client
  (package
    (name "python-google-api-python-client")
    (version "2.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "google-api-python-client" version))
        (sha256
          (base32
            "0np10mz2wj80gg7x8fvh1705g7yipi7gs81zzli05n4rczq78lib"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-google-api-core"
         ,python-google-api-core)
        ("python-google-auth" ,python-google-auth)
        ("python-google-auth-httplib2"
         ,python-google-auth-httplib2)
        ("python-httplib2" ,python-httplib2)
        ("python-six" ,python-six)
        ("python-uritemplate" ,python-uritemplate)))
    (home-page
      "https://github.com/googleapis/google-api-python-client/")
    (synopsis "Google API Client Library for Python")
    (description
      "Google API Client Library for Python")
    (license license:asl2.0)))

(define-public python-beancount
  (package
    (name "python-beancount")
    (version "2.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "beancount" version))
        (sha256
          (base32
            "1h465zc7gb0bc5pagm9fsp083sqxrn2mjfbk9l7h162xm7k8rw1b"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-bottle" ,python-bottle)
        ("python-chardet" ,python-chardet)
        ("python-dateutil" ,python-dateutil)
        ("python-google-api-python-client"
         ,python-google-api-python-client)
        ("python-lxml" ,python-lxml)
        ("python-magic" ,python-magic)
        ("python-ply" ,python-ply)
        ("python-pytest" ,python-pytest)
        ("python-requests" ,python-requests)))
    (home-page "http://furius.ca/beancount")
    (synopsis "Command-line Double-Entry Accounting")
    (description
      "Command-line Double-Entry Accounting")
    (license #f)))

(define-public python-fava
  (package
   (name "python-fava")
   (version "1.20.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "fava" version))
     (sha256
      (base32 "0dx0c57r9cn0zslwdnmsfyi7cr0jidkmcanvmmirmazymi0k1lg2"))))
   (build-system python-build-system)
   (propagated-inputs
    `(("python-babel" ,python-babel)
      ("python-beancount" ,beancount)
      ("python-cheroot" ,python-cheroot)
      ("python-click" ,python-click)
      ("python-flask" ,python-flask)
      ("python-flask-babel" ,python-flask-babel)
      ("python-jinja2" ,python-jinja2)
      ("python-markdown2" ,python-markdown2)
      ("python-ply" ,python-ply)
      ("python-simplejson" ,python-simplejson)
      ("python-typing-extensions" ,python-typing-extensions)
      ("python-werkzeug" ,python-werkzeug)))
   (home-page "https://beancount.github.io/fava/")
   (synopsis "Web interface for the accounting tool Beancount.")
   (description "Web interface for the accounting tool Beancount.")
   (license license:expat)))

python-fava
