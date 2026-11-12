(define-module (personal packages pi)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system node) #:select (node-build-system))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:use-module ((gnu packages node-xyz) #:select (node-isexe))
  #:use-module (guix utils))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/earendil-works/pi/releases/download/v"
             version "/pi-linux-x64.tar.gz"))
       (sha256
        (base32 "1d8cyx77v9va48jgkxy116i3lwgvavjm4hj0nm9f5bkskqn6y741"))))
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

(define-public node-path-key
  (package
    (name "node-path-key")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/path-key/-/path-key-"
             version ".tgz"))
       (sha256
        (base32 "14kvp849wnkg6f3dqgmcb73nnb5k6b3gxf65sgf0x0qlp6n9k2ab"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/sindresorhus/path-key")
    (synopsis "Cross-platform utility to compute the PATH environment variable key")
    (description "This package provides an implementation to compute the particular
cross-platform spellings of the PATH environment variable key.")
    (license license:expat)))

(define-public node-shebang-regex
  (package
    (name "node-shebang-regex")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/shebang-regex/-/shebang-regex-"
             version ".tgz"))
       (sha256
        (base32 "13wmb23w5srjpn9xx1c85yk5jbc5z9ypg0iz33h6nv5jdnmapnzy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (home-page "https://github.com/sindresorhus/shebang-regex")
    (synopsis "Regular expression for matching shebangs")
    (description "This package provides a regular expression for matching a
shebang line.")
    (license license:expat)))

(define-public node-shebang-command
  (package
    (name "node-shebang-command")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/shebang-command/-/shebang-command-"
             version ".tgz"))
       (sha256
        (base32 "0vjmdpwcz23glkhlmxny8hc3x01zyr6hwf4qb3grq7m532ysbjws"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list
      node-shebang-regex))
    (home-page "https://github.com/kevva/shebang-command")
    (synopsis "Get the command from a shebang")
    (description "This package extracts the command from a shebang line.")
    (license license:expat)))

(define-public node-which
  (package
    (name "node-which")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/which/-/which-"
             version ".tgz"))
       (sha256
        (base32 "1p2fkm4lr36s85gdjxmyr6wh86dizf0iwmffxmarcxpbvmgxyfm1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list
      node-isexe))
    (home-page "https://github.com/npm/node-which")
    (synopsis "Find the first instance of an executable in PATH")
    (description "Like the which(1) Unix command, find the first instance of an
executable in the PATH.")
    (license license:isc)))

(define-public node-cross-spawn
  (package
    (name "node-cross-spawn")
    (version "7.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/cross-spawn/-/cross-spawn-"
             version ".tgz"))
       (sha256
        (base32 "1siqxlydjwpihy7klgd15cah56vsmxrdm3q90gndyfj1vh63530q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)))))))
    (inputs
     (list
      node-path-key
      node-shebang-command
      node-which))
    (home-page "https://github.com/moxystudio/node-cross-spawn")
    (synopsis "Cross-platform child_process.spawn")
    (description "This package provides a cross-platform implementation of
child_process.spawn and child_process.spawnSync.")
    (license license:expat)))

(define-public pi-acp
  (package
    (name "pi-acp")
    (version "0.0.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/pi-acp/-/pi-acp-"
             version ".tgz"))
       (sha256
        (base32 "0l16ixhg3r0765cxlh0p8y784r740qczmz6p7psbglmyf6ygcfsb"))))
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
      node-cross-spawn
      node-zod))
    (home-page "https://github.com/svkozak/pi-acp")
    (synopsis "ACP adapter for pi coding agent")
    (description
     "This package provides an ACP (Agent Client Protocol) adapter for the pi
coding agent.  It exposes pi as a standalone coding agent that can be driven by
any editor supporting the Agent Client Protocol.")
    (license license:expat)))
