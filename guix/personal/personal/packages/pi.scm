(define-module (personal packages pi)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system node) #:select (node-build-system))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:use-module (guix utils))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version "0.84.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/earendil-works/pi/releases/download/v"
             version "/pi-linux-x64.tar.gz"))
       (sha256
        (base32 "0wir88dywhsclvia8hnhqfbj69qk220shg6caivdh2w5l7kc7wy2"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:patchelf-plan #~'(("pi" ()))
      #:install-plan #~'(("." "lib/pi-coding-agent/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'create-bin-symlink
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p (string-append out "/bin"))
                (symlink (string-append out "/lib/pi-coding-agent/pi")
                         (string-append out "/bin/pi"))))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://pi.dev")
    (synopsis "AI coding agent CLI with session management")
    (description
     "Pi is a coding agent CLI that provides AI-assisted software engineering
with read, bash, edit, and write tools.  It supports multiple AI providers,
session management, conversation forking, and an extension system.")
    (license license:expat)))

(define-public node-zod
  (package
    (name "node-zod")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/zod/-/zod-"
             version ".tgz"))
       (sha256
        (base32 "17171zbchqs56621d99kxgs2cg215yp879450rhh1m9zadzz2f7f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
           (add-after 'patch-dependencies 'delete-dev-dependencies
             (lambda _
               (modify-json (delete-dev-dependencies))))
           (add-after 'delete-dev-dependencies 'delete-build-scripts
             (lambda _
               (with-atomic-json-file-replacement
                 (lambda (pkg)
                   (map (lambda (kv)
                          (if (equal? (car kv) "scripts")
                              (cons "scripts"
                                    (filter
                                     (lambda (s)
                                       (not
                                        (member (car s)
                                                '("build" "prepare"
                                                  "prepack" "postinstall"))))
                                     (cdr kv)))
                              kv))
                        pkg)))))
           (delete 'build))))
    (native-inputs
     (list))
    (home-page "https://www.npmjs.com/package/zod")
    (synopsis "TypeScript-first schema validation")
    (description "Zod is a TypeScript-first schema validation library with static type
inference.")
    (license license:expat)))

(define-public node-agentclientprotocol-sdk
  (package
    (name "node-agentclientprotocol-sdk")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@agentclientprotocol/sdk/-/sdk-"
             version ".tgz"))
       (sha256
        (base32 "17ycpr4s4yrr6r8gvzyjd63zh5aawjvi20lwk2zh0bc4w5mmpbqb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
           (add-after 'patch-dependencies 'delete-dev-dependencies
             (lambda _
               (modify-json (delete-dev-dependencies))))
           (add-after 'delete-dev-dependencies 'delete-build-scripts
             (lambda _
               (with-atomic-json-file-replacement
                 (lambda (pkg)
                   (map (lambda (kv)
                          (if (equal? (car kv) "scripts")
                              (cons "scripts"
                                    (filter
                                     (lambda (s)
                                       (not
                                        (member (car s)
                                                '("build" "prepare"
                                                  "prepack" "postinstall"))))
                                     (cdr kv)))
                              kv))
                        pkg)))))
           (delete 'build))))
    (native-inputs
     (list))
    (inputs
     (list
      node-zod))
    (home-page "https://www.npmjs.com/package/@agentclientprotocol/sdk")
    (synopsis "Agent Client Protocol SDK")
    (description "This package implements the Agent Client Protocol (ACP), which
standardizes communication between code editors and AI coding agents.")
    (license license:asl2.0)))

(define-public pi-acp
  (package
    (name "pi-acp")
    (version "0.0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/pi-acp/-/pi-acp-"
             version ".tgz"))
       (sha256
        (base32 "0jyy62bdsf9kgxsmgaz111rh0wj411cz6hkjq0r6n18cg2kbiplz"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
           (add-after 'patch-dependencies 'delete-dev-dependencies
             (lambda _
               (modify-json (delete-dev-dependencies))))
           (delete 'build))))
    (inputs
     (list
      node-agentclientprotocol-sdk
      node-zod))
    (home-page "https://github.com/svkozak/pi-acp")
    (synopsis "ACP adapter for pi coding agent")
    (description
     "This package provides an ACP (Agent Client Protocol) adapter for the pi
coding agent.  It exposes pi as a standalone coding agent that can be driven by
any editor supporting the Agent Client Protocol.")
    (license license:expat)))
