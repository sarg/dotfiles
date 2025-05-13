(define-module (personal packages secretsd)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (guix packages))

(define-public secretsd
  (let ((commit "4ea56226b8f7c8739eea7fc8d1ffca8e18cf58c9")
        (revision "0"))
    (package
      (name "secretsd")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/grawity/secretsd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ka21vmvm25kal3sa8zmrifh4zac878hk24y7y3jj3ig8dkv0vfy"))))
      (build-system python-build-system)
      (arguments
       '(#:tests? #f                    ; TODO: enable
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-deps-install
             (lambda _
               (substitute* "setup.py"
                 ((".*install_requires.*") ""))))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/secretsd")
                   `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))))))))
      (inputs (list
               python-dbus
               python-platformdirs
               python-cryptography
               python-xdg
               python-pygobject))
      (native-inputs (list python-setuptools python-wheel))
      (home-page "https://github.com/grawity/secretsd")
      (synopsis "A basic FreeDesktop.org Secret Service backend")
      (description "A generic backend for the libsecret API to use on headless systems or minimal
desktop environments.")
      (license license:expat))))
