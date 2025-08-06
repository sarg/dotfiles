(define-module (personal packages binary)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages))

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
   (version "5.1.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://archive.tinymediamanager.org/v" version
                                "/tinyMediaManager-" version "-linux-amd64.tar.xz"))
            (sha256
             (base32 "1ngqcq36h1569dcj4adgaz24gh0wcif6ks0srz0acg5rvxhsr1ag"))))
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

(define-public git-bug
  (package
    (name "git-bug")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/git-bug/git-bug/releases/download/v" version "/git-bug_linux_amd64"))
       (sha256 "138n8wrr6z633gfw699mg9hzvvh126rh9gda1rx866l8bxjn12ck")))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan #~'(("git-bug" ()))
      #:install-plan #~'(("git-bug" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (copy-file source "./git-bug")
              (chmod "git-bug" #o644)))
          (add-before 'install 'chmod
            (lambda* (#:key inputs #:allow-other-keys)
              (chmod "git-bug" #o555))))))
    (inputs (list glibc))
    (home-page "https://github.com/git-bug/git-bug")
    (synopsis "a decentralized issue tracker")
    (description "git-bug is a standalone, distributed, offline-first issue management tool that
embeds issues, comments, and more as objects in a git repository (not files!),
enabling you to push and pull them to one or more remotes.")
    (license license:gpl3+)))

(define-public terraform
  (package
   (name "terraform")
   (version "1.6.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://releases.hashicorp.com/terraform/"
                                version "/terraform_" version "_linux_amd64.zip"))
            (sha256
             (base32 "19p2jbfg76663q4vanr02f97l6zaf7s2g59c65496kf41j2pi9yi"))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux" "i686-linux"))
   (arguments
    `(#:install-plan '(("terraform" "bin/"))))
   (native-inputs (list unzip))
   (synopsis "Terraform is a tool for building, changing, and versioning infrastructure safely and efficiently.")
   (description "Terraform enables you to safely and predictably create, change, and improve infrastructure. It is an open source tool that codifies APIs into declarative configuration files that can be shared amongst team members, treated as code, edited, reviewed, and versioned.")
   (home-page "https://www.terraform.io/")
   (license #f)))

(define-public google-cloud-sdk
  (package
    (name "google-cloud-sdk")
    (version "465.0.0")
    (source (origin
             (method url-fetch)
             ;; A starting point for a proper package is here:
             ;; https://storage.googleapis.com/cloud-sdk-release/for_packagers
             ;; /linux/google-cloud-sdk_337.0.0.orig.tar.gz
             (uri (string-append
                   "https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/"
                   "google-cloud-sdk-" version "-linux-x86_64.tar.gz"))
             (sha256
              (base32 "0mp71q62yj6xmf1n94myq6dzvpjmxc5fikd9gkvh28hwx7q8w2by"))))
    ;; We use the GNU build system mainly for its patch-shebang phases.
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; This is just copying a binary, so no tests to perform.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configuration, just copying.
         (delete 'build)     ; No building, just copying.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (bin      (string-append out "/bin"))
                    (lib      (string-append out "/lib"))
                    (platform (string-append out "/platform"))
                    (share    (string-append out "/share/google-cloud-sdk")))
               (for-each mkdir-p (list out share))
               (copy-recursively "bin" bin)
               (copy-recursively "lib" lib)
               (copy-recursively "platform" platform)))))))
    (propagated-inputs
     (list python coreutils))
    (home-page "https://cloud.google.com/sdk")
    (synopsis "Google Cloud SDK")
    (description "This package provides the Google Cloud SDK which includes the
command-line programs gsutil and gcloud among others.")
    (license license:asl2.0)))

(define-public terraform-ls
  (package
    (name "terraform-ls")
    (version "0.36.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://releases.hashicorp.com/terraform-ls/" version
             "/terraform-ls_" version "_linux_amd64.zip"))
       (sha256
        (base32 "08wjzabcmjfy8jnng5xh9xvrp6ffsg5gxvkblfkh4rgyblnibx4a"))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments (list
                #:install-plan
                #~(list '("terraform-ls" "bin/"))))
    (native-inputs (list unzip))
    (home-page "https://github.com/hashicorp/terraform-ls")
    (synopsis "Language server for terraform")
    (description "The official Terraform language server maintained by HashiCorp provides IDE features to any LSP-compatible editor.")
    (license license:mpl2.0)))

(define-public pulumi-esc
  (package
    (name "pulumi-esc")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://get.pulumi.com/esc/releases/esc-v" version "-linux-x64.tar.gz"))
       (sha256
        (base32 "1q4g4gab601acvyrg5p4ry4r3kkhkz9i37d6a78blfr122pbvmbk"))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:install-plan '(("." "bin/"))))
    (home-page "https://www.pulumi.com")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public pulumi
  (package
    (name "pulumi")
    (version "3.177.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://get.pulumi.com/releases/sdk/pulumi-v" version "-linux-x64.tar.gz"))
       (sha256
        (base32 "1wzvhngr12pvrnwn375d8qbil0b17nxil2apmyrbwmdpry4a2sd6"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:install-plan '(("." "bin/"))
       #:patchelf-plan '(("pulumi-watch" ("glibc" "gcc:lib")))))
    (inputs `(("glibc" ,glibc)
              ("gcc:lib" ,gcc "lib")))
    (home-page "https://www.pulumi.com")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public httptap
  (package
   (name "httptap")
   (version "0.1.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/monasticacademy/httptap/releases/download/v"
           version "/httptap_linux_x86_64.tar.gz"))
     (sha256
      (base32 "1vyzlg4h5g71qgbh0zsbfjq1ll3mc4a51mnz055ydhx4dnq97val"))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:install-plan '(("httptap" "bin/"))))
   (home-page "https://github.com/monasticacademy/httptap")
   (synopsis #f)
   (description #f)
   (license license:expat)))

(define-public jujutsu
  (package
   (name "jujutsu")
   (version "0.31.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/jj-vcs/jj/releases/download/v" version "/jj-v" version "-x86_64-unknown-linux-musl.tar.gz"))
     (sha256
      (base32 "0487k053kl9ifz4bx19af3as6zr612r1ifr0skl9cw31x30xh5xj"))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:install-plan '(("jj" "bin/"))))
   (home-page "https://github.com/jj-vcs/jj")
   (synopsis #f)
   (description #f)
   (license license:asl2.0)))
