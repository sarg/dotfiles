(define-module (personal packages binary)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages java)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages node)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages))

(define-public restic
  (let* ((version "0.18.1")
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
                 (base32 "0g9n3z28p4mss4z7hd8prwcjrwcavl65gq97lbdil5b7kpqkh238"))))
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
   (version "5.2.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://archive.tinymediamanager.org/v" version
                                "/tinyMediaManager-" version "-linux-amd64.tar.xz"))
            (sha256
             (base32 "1va8z8msfa4gsswvgmizjxshzwwqizm9n3ja8r266arh2g66gvgk"))))
   (build-system binary-build-system)
   (inputs (list libmediainfo openjdk))
   (supported-systems '("x86_64-linux"))
   (properties `((release-monitoring-url . "https://archive.tinymediamanager.org/")
                 (release-file-regexp . "v([0-9.]*)/$")))
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
    (version "1.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/temporal/releases/download/v"
             version "/temporal_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0y1yppr21wcdynx5ivkyj7522kv4j721rhn1pc5yam3h7300bpx5"))))
    (properties '((upstream-name . "temporal")))
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
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/cli/releases/download/v"
             version "/temporal_cli_" version "_linux_amd64.tar.gz"))
       (sha256 "0bc5nic778yjdihlx0zqfpl2nzgbi3sa6cwpxbafyxmhn045xjfx")))
    (properties '((upstream-name . "temporal_cli")))
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
    (version "2.86.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cli/cli/releases/download/v" version
                           "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "1iq3h6k1q1612is2jjzhscwbiwi5za3sd8dh54icq844lbb8pc7k"))))
    (properties '((upstream-name . "gh")))
    (build-system binary-build-system)
    (home-page "https://github.com/cli/cli")
    (supported-systems '("x86_64-linux"))
    (synopsis "GitHub's official command line tool")
    (description "gh is GitHub on the command line. It brings pull requests,
issues, and other GitHub concepts to the terminal next to where you are already
working with git and your code.")
    (license license:expat)))

(define-public atuin
  (package
   (name "atuin")
   (version "18.11.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/atuinsh/atuin/releases/download/v"
                         version
                         "/atuin-x86_64-unknown-linux-gnu.tar.gz"))
     (sha256 (base32 "16jwjmbw32j0i3znpfzdq3qls4cxvbcrcnj3wa7kg86jx4zzfwzl"))))
   (supported-systems '("x86_64-linux"))
   (build-system binary-build-system)
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (arguments
    `(#:strip-binaries? #f
      #:install-plan '(("atuin-x86_64-unknown-linux-gnu/atuin" "bin/"))
      #:patchelf-plan '(("atuin-x86_64-unknown-linux-gnu/atuin" ("glibc" "gcc:lib")))
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
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/git-bug/git-bug/releases/download/v" version "/git-bug_linux_amd64"))
       (sha256 "18al1pnryq5x748pna0saav5privh8q50pc2dqdyyvsj3ssgi8iv")))
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

(define-public codelldb
  (package
   (name "codelldb")
   (version "1.12.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/vadimcn/codelldb/releases/download/v" version "/codelldb-linux-x64.vsix"))
            (sha256
             (base32 "0w15rp0k3spxhjjw4bmzhbfjlbwmcn74h2mic5cwmbsvx8m6k5a7"))))
   (build-system binary-build-system)
   (arguments
    `(#:strip-binaries? #f
      #:patchelf-plan `(("extension/adapter/codelldb" ("glibc" "gcc:lib"))
                        ("extension/bin/codelldb-launch" ("glibc" "gcc:lib")))
      #:phases (modify-phases %standard-phases
                 (replace 'unpack
                   (lambda* (#:key inputs source #:allow-other-keys)
                     (invoke "unzip" source)))
                 (add-after 'unpack 'clean
                        (lambda _
                          (delete-file-recursively "extension/lldb"))))))
   (native-inputs (list unzip))
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/vadimcn/codelldb")
   (synopsis "A VSCode debugger extension for native code, powered by LLDB.")
   (description "VSCode extension for debugging")
   (license license:expat)))

(define-public ddnet
  (package
    (name "ddnet")
    (version "19.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ddnet.org/downloads/DDNet-" version "-linux_x86_64.tar.xz"))
       (sha256
        (base32 "1bhqzgf646nr54hc9abhq4d87d3bsr1113n6f9fy9kic9kcpwin8"))))
    (build-system binary-build-system)
    (arguments
     (list #:strip-binaries? #f
           #:validate-runpath? #f
           #:install-plan ''(("DDNet" "bin/")
                             ("DDNet-Server" "bin/")
                             ("data/" "share/ddnet/"))
           #:patchelf-plan ''(("DDNet"
                               ("vulkan-loader" "libnotify" "mesa" "freetype" "curl" "glib" "sdl2"))
                              ("DDNet-Server" ("curl")))

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-maps
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((share (string-append #$output "/share/ddnet"))
                          (maps (assoc-ref inputs "ddnet-maps")))
                     (delete-file (string-append share "/autoexec_server.cfg"))
                     (for-each (lambda (f)
                                 (symlink (string-append maps f)
                                          (string-append share f)))
                               '("/types" "/autoexec_server.cfg" "/reset.cfg" "/storage.cfg")))))
               (add-after 'install-maps 'create-desktop-entry
                 (lambda _
                   (let* ((share (string-append #$output "/share"))
                          (data (string-append share "/ddnet")))
                     (make-desktop-entry-file
                      (string-append share "/applications/ddnet.desktop")
                      #:name "DDNet"
                      #:path data
                      #:icon (string-append data "/deadtee.png")
                      #:exec (string-append #$output "/bin/DDNet")
                      #:keywords '("game" "multiplayer")
                      #:categories '("Game" "ArcadeGame"))))))))
    (native-inputs
     (list
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ddnet/ddnet-maps")
               (commit "48e8ae016b1892f771819368e31b94d68aaadce3")))
        (file-name "ddnet-maps")
        (sha256
         (base32 "132vq839ajb0pcnaqrg9djvcynkd8iab93fa88irkq4l7j0s22c2")))))
    (inputs (list vulkan-loader libnotify mesa freetype curl glib sdl2))
    (supported-systems '("x86_64-linux"))
    (properties '((release-monitoring-url . "https://ddnet.org/downloads/")))
    (home-page "https://ddnet.org/")
    (synopsis "DDraceNetwork (DDNet), a cooperative platformer game")
    (description "DDraceNetwork (DDNet) is an actively maintained version of DDRace, a Teeworlds
modification with a unique cooperative gameplay.")
    (license license:zlib)))

(define-public python-ty
  (package
    (name "python-ty")
    (version "0.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/astral-sh/ty/releases/download/" version
             "/ty-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "0361n2xrvl1z94dlfzrpnwz5ca5xrlars6lqxbwn5dyww4qxlm0a"))))
    (properties '((upstream-name . "ty")))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:install-plan '(("ty" "bin/"))
       #:validate-runpath? #f
       #:patchelf-plan '(("ty" ("gcc:lib")))))
    (inputs `(("gcc:lib" ,gcc "lib")))
    (home-page "https://github.com/astral-sh/ty")
    (synopsis "python language server")
    (description "ty is an extremely fast Python type checker and language server, written in Rust.")
    (license license:expat)))

(define-public goose
  (package
    (name "goose")
    (version "1.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/block/goose/releases/download/v"
                           version "/goose-x86_64-unknown-linux-gnu.tar.bz2"))
       (sha256
        (base32 "1hqrb6yx3wkwfwaydpsmka9zixgf2vhkjrlf9rwy34mc2c9a8z4h"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:validate-runpath? #f
       #:install-plan '(("goose" "bin/"))
       #:patchelf-plan '(("goose" ("glibc" "gcc:lib" "libxcb")))))
    (inputs `(("gcc:lib" ,gcc "lib")
              ("glibc" ,glibc)
              ("libxcb" ,libxcb)))
    (home-page "https://github.com/block/goose")
    (synopsis "CLI AI coding agent")
    (description "Goose is an extensible AI agent that goes beyond code suggestions - install, execute, edit, and test with any LLM")
    (license license:asl2.0)))

(define-public font-nerd-fonts-symbols
  (package
    (name "font-nerd-fonts-symbols")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ryanoasis/nerd-fonts/"
                           "releases/download/v" version
                           "/NerdFontsSymbolsOnly.tar.xz"))
       (sha256
        (base32 "0skirmz6rc0845960957b19kvlbfpg5k9gs6hq8agsmhlc6hk33z"))))
    (build-system font-build-system)
   (properties `((upstream-name . "NerdFontsSymbolsOnly")))
   (home-page "https://www.nerdfonts.com/")
    (synopsis "Nerd Font including only the symbols")
    (description "Nerd Font that includes only the icons.")
    (license license:silofl1.1)))

(define-public font-emojione-color
  (package
    (name "font-emojione-color")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/adobe-fonts/emojione-color")
              (commit "835b4ef8384f55ecf9abf7ecc943a3980884690b")))
       (sha256
        (base32 "1sc260ziqyr2q7rxh7n5bhy4czv2ny06pj79q68y3h0r9gsfgkjh"))
       (modules '((guix build utils)))
       (snippet '(delete-file "EmojiOneBW.otf"))))
    (build-system font-build-system)
    (home-page "https://www.emojione.com/emoji/v2")
    (synopsis "EmojiOne color font")
    (description "EmojiOne color font")
    (license license:expat)))

(define-public google-gemini-cli
  (package
    (name "google-gemini-cli")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/google-gemini/gemini-cli/releases/download/v" version "/gemini.js"))
       (sha256
        (base32 "1njb51b2k33gllg0dbdkj1bsgf58bbn2jsrwskwf8nv9v6scd74k"))))
    (build-system binary-build-system)
    (arguments (list
                #:install-plan
                #~(list '("gemini.js" "bin/gemini"))
                #:phases
                #~(modify-phases %standard-phases
                    (add-before 'install 'chmod
                      (lambda _
                        (chmod "gemini.js" #o555))))))
    (properties '((upstream-name . "gemini")))
    (inputs (list node))
    (home-page "https://geminicli.com/")
    (synopsis "Gemini AI agent")
    (description "Duh, it's an AI agent")
    (license license:asl2.0)))
