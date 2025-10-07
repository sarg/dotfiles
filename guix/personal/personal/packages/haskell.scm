(define-module (personal packages haskell)
  #:use-module (srfi srfi-26)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix packages))

(define-public oama
  (package
    (name "oama")
    (version "0.22.0")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/pdobsan/oama")
             (commit version)))
       (sha256
        (base32 "1lasr8psfsgc43in6lgaf7byvmdvanhg7idxijz504z6ga7v0pnj"))
       (patches (list
                 (search-path
                  (map (cut string-append <> "/personal/packages/patches")
                       %load-path)
                  "oama-adapt-for-guix-packaging.patch")))))
    (build-system haskell-build-system)
    (inputs
     (list
      ghc-aeson
      ghc-base64-bytestring
      ghc-cryptohash-sha256
      ghc-githash
      ghc-hsyslog
      ghc-http-conduit
      ghc-http-types
      ghc-network
      ghc-network-uri
      ghc-optparse-applicative
      ghc-pretty-simple
      ghc-random
      ghc-streaming-commons
      ghc-string-qq
      ghc-strings
      ghc-twain
      ghc-utf8-string
      ghc-warp
      ghc-yaml))
    (home-page "https://github.com/pdobsan/oama")
    (synopsis "OAuth credential MAnager")
    (description "Provide OAuth2 capabilities to IMAP/SMTP clients.")
    (license license:bsd-3)))

(define-public ghc-hsyslog
  (package
    (name "ghc-hsyslog")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hsyslog" version))
       (sha256
        (base32 "1kkypn0dd92aqv8dr112bhkr9k9r9mchnyyvy41kvhj2zg447v1y"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hsyslog")))
    (arguments
     `(#:cabal-revision ("1"
                         "0k1j46nk3z64zw4bqmvw5lgy16ih200m66rv4b6ygjqv7nglqq0b")))
    (home-page "https://github.com/peti/hsyslog")
    (synopsis "FFI interface to syslog(3) from POSIX.1-2001")
    (description
     "This package provides a Haskell interface to @@syslog(3)@@ as specified in
<http://pubs.opengroup.org/onlinepubs/9699919799/functions/syslog.html
POSIX.1-2008>.  The entire public API lives in \"System.Posix.Syslog\".  There is
a set of exposed modules available underneath that one, which contain various
implementation details that may be useful to other developers who want to
implement syslog-related functionality. /Users/ of @@syslog@@, however, do not
need them. .  An example program that demonstrates how to use this library is
available in the <https://github.com/peti/hsyslog/blob/master/example/Main.hs
examples> directory of this package.")
    (license license:bsd-3)))

(define-public ghc-strings
  (package
    (name "ghc-strings")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "strings" version))
       (sha256
        (base32 "1xz9v3w5s13yhk7iy9dw6i8s2jc6c0b1ci96dwmcq9a1n3l3ng4v"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "strings")))
    (arguments
     `(#:cabal-revision ("1"
                         "0jk1g71yzc5wpkr3vvhnxak61nqvisc5n90ggv6lmz4wqpqzdd0v")))
    (home-page "http://hub.darcs.net/scravy/strings")
    (synopsis
     "Functions for working with strings, including Text, ByteString, etc.")
    (description
     "This package provides various functions for working with strings, such as
@@join@@, @@split@@, @code{@@toUppercase}@@, etc. .  The functions in this
package work with all kinds of strings such as Text, @code{ByteString}, String,
and their respective lazy counter parts.  There is also an interface which is
agnostic of the underlying string type. . [@@v1.0.2@@] Fixed an issue with
@code{@@strSplitAll}@@.  Applied to the empty string it should return an empty
list, but it returned a list containing a single empty string.  It now returns
correctly the empty list. . [@@v1.1@@] Added @code{@@strToUpper}@@,
@code{@@strToLower}@@, @code{@@strCapitalize}@@, @code{@@strCons}@@, and
@code{@@strMap}@@.  Also @code{@@sCapitalize}@@, @code{@@sCons}@@, and
@code{@@sMap}@@.")
    (license license:expat)))

(define-public ghc-twain
  (package
    (name "ghc-twain")
    (version "2.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "twain" version))
       (sha256
        (base32 "1hkzp2g671dagmv1qznkf3mw3l2mslckg7h0a8x8633h6i3j6br0"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "twain")))
    (inputs (list ghc-aeson
                  ghc-case-insensitive
                  ghc-cookie
                  ghc-either
                  ghc-http-types
                  ghc-http2
                  ghc-vault
                  ghc-wai
                  ghc-wai-extra))
    (native-inputs (list ghc-hspec ghc-hspec-discover ghc-hspec-wai))
    (home-page "https://github.com/alexmingoia/twain#readme")
    (synopsis "Tiny web application framework for WAI.")
    (description
     "Twain is tiny web application framework for WAI. It provides routing, parameter
parsing, and an either-like monad for composing responses.")
    (license license:bsd-3)))

