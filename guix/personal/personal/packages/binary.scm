(define-module (personal packages binary)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages video)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define-public babashka
  (package
   (name "babashka")
   (version "1.12.197")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/babashka/babashka/releases/download/v"
                  version "/babashka-" version "-linux-amd64.tar.gz"))
            (sha256
             (base32 "0p3025pnf58218ngjdz9kcpr43lyh1c0dljs5ahgyjmpm62lyh44"))))
   (build-system binary-build-system)
   (inputs (list zlib openjdk))
   (supported-systems '("x86_64-linux"))
   (arguments
    (list
     #:install-plan #~'(("bb" "bin/bb"))
     #:patchelf-plan #~'(("bb" ("zlib")))

     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda _
             (wrap-program (string-append #$output "/bin/bb")
               `("JAVA_HOME" = (#$openjdk))))))))
   (home-page "https://babashka.org")
   (synopsis "babashka clojure scripting runtime")
   (description "babashka clojure scripting runtime")
   (license license:epl1.0)))

(define-public oama
  (package
    (name "oama")
    (version "0.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pdobsan/oama/releases/download/"
                    version "/oama-" version "-Linux-x86_64.tar.gz"))
              (sha256
               (base32 "01qn02qrsa4rngbd9vg3h0sa2zhhs4npkjcr95n0bz0b3jhqfzg4"))))

    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments (list
                #:install-plan
                #~(list '("oama" "bin/oama"))))
    (home-page "https://github.com/pdobsan/oama")
    (synopsis "OAuth credential MAnager")
    (description "Provide OAuth2 capabilities to IMAP/SMTP clients.")
    (license license:bsd-3)))

(define-public restic
  (let* ((version "0.18.0")
         (pkg (string-append "restic_" version "_linux_amd64")))
    (package
      (name "restic")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/restic/restic/releases/download/v"
                      version "/" pkg ".bz2"))
                (sha256
                 (base32 "13j3fhip4fss4y8fxk360rl2mk4nw5cap3gx9g85i45myn5xvxlq"))))
      (build-system binary-build-system)
      (supported-systems '("x86_64-linux"))
      (arguments (list
                  #:install-plan
                  #~(list '(#$pkg "bin/restic"))
                  #:phases
                  #~(modify-phases %standard-phases
                      (add-before 'install 'chmod
                        (lambda _
                          (chmod #$pkg #o555))))))
      (home-page "https://restic.net")
      (synopsis "backup software")
      (description "backup software")
      (license license:bsd-2))))

(define-public tinymediamanager
  (package
   (name "tinymediamanager")
   (version "5.1.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://release.tinymediamanager.org/v5/dist/tinyMediaManager-"
                                version "-linux-amd64.tar.xz"))
            (sha256
             (base32 "0im7ifmyx2czf44d35dsv0shmd9p6rpkxyhhh9k3iy2y283xy5h0"))))
   (build-system binary-build-system)
   (inputs (list libmediainfo openjdk))
   (supported-systems '("x86_64-linux"))
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'create-runner
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append #$output "/bin"))
                    (lib (string-append #$output "/lib/tinyMediaManager"))
                    (tmm (string-append bin "/tinyMediaManager")))
               (mkdir-p bin)
               (call-with-output-file tmm
                 (lambda (out)
                   (format out "#!/bin/sh
LD_LIBRARY_PATH=~a CLASSPATH=~a/* ~a/bin/java ~a org.tinymediamanager.TinyMediaManager"
                           (string-join
                            (list
                             (string-append (assoc-ref inputs "libmediainfo") "/lib")
                             (string-append (assoc-ref inputs "libzen") "/lib"))
                            ":")
                           lib
                           (assoc-ref inputs "openjdk")
                           (string-join
                            '("-Xms64m"
                              "-Xmx512m"
                              "-Xss512k"
                              "-Dsun.java2d.renderer=sun.java2d.marlin.MarlinRenderingEngine"
                              "-Djava.net.preferIPv4Stack=true"
                              "-Dfile.encoding=UTF-8"
                              "-Dsun.jnu.encoding=UTF-8"
                              "-Dtmm.consoleloglevel=NONE"
                              "-Dawt.useSystemAAFontSettings=on"
                              "-Dswing.aatext=true"
                              "-Dtmm.contentfolder=$XDG_DATA_HOME/tinyMediaManager"
                              "-Dtmm.datafolder=$XDG_STATE_HOME/tinyMediaManager"
                              "-Dtmm.noupdate=true")
                            " "))))
               (chmod tmm #o555))

             (make-desktop-entry-file
              (string-append #$output "/share/applications/tinymediamanager.desktop")
              #:name "Tiny Media Manager"
              #:type "Application"
              #:exec (string-append #$output "/bin/tinyMediaManager")
              #:icon "tmm"
              #:categories '("Video" "Database")
              #:terminal #f
              #:comment
              '(("en" "Media collection manager")
                (#f "Media collection manager"))))))
     #:validate-runpath? #f
     #:install-plan #~'(("tmm.png" "share/icons/hicolor/128x128/apps/")
                        ("lib" "lib/tinyMediaManager")
                        ("tmm.jar" "lib/tinyMediaManager/"))))
   (home-page "https://tinymediamanager.org")
   (synopsis "Media library manager")
   (description "Media library manager")
   (license license:asl1.1)))

(define-public temporal-io-server
  (package
    (name "temporal-io-server")
    (version "1.27.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/temporal/releases/download/v"
             version "/temporal_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0r7xqila3vr6gwy3d9bwp3rzvw67fa7wxc794b2gq498pp8vqzpv"))))
    (build-system copy-build-system)
    (arguments
     (list

     #:phases
     #~(modify-phases %standard-phases
         (add-after 'unpack 'get-back
           (lambda _ (chdir ".."))))

      #:install-plan
      #~'(("." "bin/" #:include-regexp ("temporal-.*")))))
    (home-page "https://go.temporal.io/server")
    (synopsis "Temporal")
    (description
     "Temporal is a durable execution platform that enables developers to build
scalable applications without sacrificing productivity or reliability.  The
Temporal server executes units of application logic called Workflows in a
resilient manner that automatically handles intermittent failures, and retries
failed operations.")
    (license license:expat)))

(define-public temporal-cli
  (package
    (name "temporal-cli")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/cli/releases/download/v"
             version "/temporal_cli_" version "_linux_amd64.tar.gz"))
       (sha256 "0knpzhylx8c3nilflxw5949mg4b5zddbbnkm817hids8p5prf0ya")))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("temporal" "bin/"))))
    (home-page "https://go.temporal.io/cli")
    (synopsis "Temporal")
    (description
     "Temporal is a durable execution platform that enables developers to build
scalable applications without sacrificing productivity or reliability.  The
Temporal server executes units of application logic called Workflows in a
resilient manner that automatically handles intermittent failures, and retries
failed operations.")
    (license license:expat)))

(define-public github-cli
  (package
    (name "github-cli")
    (version "2.72.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "https://github.com/cli/cli/releases/download/v" version
                           "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0f3zqwab3mplzgxsfx96s67g6n76aijv00hkaccn3kvm21wsklzz"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      (let ((dir (string-append "gh_" version "_linux_amd64")))
        #~'((#$(string-append dir "/bin") "bin")
            (#$(string-append dir "/share") "share")))))
    (home-page "https://github.com/cli/cli")
    (supported-systems '("x86_64-linux"))
    (synopsis "GitHub’s official command line tool")
    (description "gh is GitHub on the command line. It brings pull requests,
issues, and other GitHub concepts to the terminal next to where you are already
working with git and your code.")
    (license license:expat)))

(define-public atuin
  (package
   (name "atuin")
   ;; Use revision helper?
   (version "18.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/atuinsh/atuin/releases/download/"
                         version
                         "/atuin-x86_64-unknown-linux-gnu.tar.gz"))
     (sha256 (base32 "0fdrw97hnxy62mpparbxax6r0x0kclv6md61mkr4h2iz1q94ysgs"))))
   (supported-systems '("x86_64-linux"))
   (build-system binary-build-system)
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (arguments
    `(#:strip-binaries? #f
      #:install-plan '(("atuin-x86_64-unknown-linux-gnu/atuin" "bin/"))
      #:patchelf-plan `(("atuin-x86_64-unknown-linux-gnu/atuin" ("glibc" "gcc:lib")))
      #:phases (modify-phases %standard-phases
                 (replace 'unpack
                   (lambda* (#:key inputs source #:allow-other-keys)
                     (invoke "tar" "-zxf" source))))))
   (home-page "https://atuin.sh/")
   (synopsis "Sync, search and backup shell history")
   (description "Atuin lets you sync, search and backup shell history. It stores your shell
history in an SQLite database.")
   (license license:expat)))
