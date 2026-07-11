(define-module (personal packages binary)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (nongnu packages mozilla)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages))

(define-public tinymediamanager
  (package
   (name "tinymediamanager")
   (version "5.2.12")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://archive.tinymediamanager.org/v" version
                                "/tinyMediaManager-" version "-linux-amd64.tar.xz"))
            (sha256
             (base32 "1wy4jkvgc3r6vkfcd5a88jdqxb49yji3s5adi219pn4q954p7znj"))))
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
    (version "1.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/temporal/releases/download/v"
             version "/temporal_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0qxsyxkmpjrm9msfw0ahlv6jbhzpblv9hmsrivjc8iihg2lbws3d"))))
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
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/temporalio/cli/releases/download/v"
             version "/temporal_cli_" version "_linux_amd64.tar.gz"))
       (sha256 "0nrpf83inr3mqgizv03avdyap8g6lihn9z153zbvf878hjplixg2")))
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
    (version "3.220.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://get.pulumi.com/releases/sdk/pulumi-v" version "-linux-x64.tar.gz"))
       (sha256
        (base32 "1zs5mpy4rcgv0fadbb6pv95kq75152rpngch676j2n63w2awa03g"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:install-plan '(("." "bin/"))
       #:patchelf-plan '(("pulumi-watch" ("libc" "gcc:lib")))))
    (inputs `(("gcc:lib" ,gcc "lib")))
    (home-page "https://www.pulumi.com")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public codelldb
  (package
   (name "codelldb")
   (version "1.12.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/vadimcn/codelldb/releases/download/v" version "/codelldb-linux-x64.vsix"))
            (sha256
             (base32 "0zb3gzrzgyj78q35jdf7ashg6f8wl6fnqz4jn0sxal80ayl4anxq"))))
   (build-system binary-build-system)
   (arguments
    `(#:strip-binaries? #f
      #:patchelf-plan `(("extension/adapter/codelldb" ("libc" "gcc:lib"))
                        ("extension/bin/codelldb-launch" ("libc" "gcc:lib")))
      #:phases (modify-phases %standard-phases
                 (replace 'unpack
                   (lambda* (#:key inputs source #:allow-other-keys)
                     (invoke "unzip" source)))
                 (add-after 'unpack 'clean
                        (lambda _
                          (delete-file-recursively "extension/lldb"))))))
   (native-inputs (list unzip))
   (inputs `(("gcc:lib" ,gcc "lib")))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/vadimcn/codelldb")
   (synopsis "A VSCode debugger extension for native code, powered by LLDB.")
   (description "VSCode extension for debugging")
   (license license:expat)))

(define-public ddnet
  (package
    (name "ddnet")
    (version "19.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ddnet.org/downloads/DDNet-" version "-linux_x86_64.tar.xz"))
       (sha256
        (base32 "01zv0sqwxqv4c6vnv06zc875hgqzs6np4nb82k29gl049awva320"))))
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
    (version "0.0.57")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/astral-sh/ty/releases/download/" version
             "/ty-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "07202fxjxiwrav920clc4ap7lzgfcnq0a55846im4dw2kp6bg95d"))))
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
       #:patchelf-plan '(("goose" ("libc" "gcc:lib" "libxcb")))))
    (inputs `(("gcc:lib" ,gcc "lib") ("libxcb" ,libxcb)))
    (home-page "https://github.com/block/goose")
    (synopsis "CLI AI coding agent")
    (description "Goose is an extensible AI agent that goes beyond code suggestions - install, execute, edit, and test with any LLM")
    (license license:asl2.0)))

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
    (version "0.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/google-gemini/gemini-cli/releases/download/v" version "/gemini-cli-bundle.zip"))
       (sha256
        (base32 "1qm3zmwf63xixs4s1z14pkzznpvz9cxin5xym4byw4d3pc0zsv4p"))))
    (build-system binary-build-system)
    (arguments (list
                #:install-plan ''(("." "share/"))
                #:phases
                #~(modify-phases %standard-phases
                    (add-before 'install 'set-node
                      (lambda _
                        (chdir "..")
                        (substitute* "gemini.js"
                          (("^#!/usr/bin/env node")
                           (string-append "#!" (which "node"))))))
                    (add-after 'install 'install-bin
                      (lambda _
                        (mkdir-p (string-append #$output "/bin"))
                        (symlink (string-append #$output "/share/gemini.js")
                                 (string-append #$output "/bin/gemini")))))))
    (properties '((upstream-name . "gemini")))
    (native-inputs (list unzip))
    (inputs (list node))
    (home-page "https://geminicli.com/")
    (synopsis "Gemini AI agent")
    (description "Duh, it's an AI agent")
    (license license:asl2.0)))

(define-public opencode
  (package
    (name "opencode")
    (version "1.14.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/anomalyco/opencode/releases/download/v" version "/opencode-linux-x64.tar.gz"))
       (sha256
        (base32 "0448q9arzb3ivd9817fwh30zip4hrw8654blxhck8mv71baf6gdb"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     (list
      #:patchelf-plan #~'()
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-and-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (orig (string-append out "/opencode"))
                     (bash (search-input-file inputs "bin/bash"))
                     (patchelf (search-input-file inputs "bin/patchelf"))
                     (ld.so (search-input-file inputs "lib/ld-linux-x86-64.so.2"))
                     (libpath (string-join
                               (list (string-append (assoc-ref inputs "gcc") "/lib")
                                     (string-append (assoc-ref inputs "glibc") "/lib")
                                     (string-append (assoc-ref inputs "libx11") "/lib")
                                     (string-append (assoc-ref inputs "mesa") "/lib"))
                               ":")))
                ;; Only patch interpreter; full patchelf corrupts this binary.
                (invoke patchelf "--set-interpreter" ld.so orig)
                (rename-file orig (string-append orig ".real"))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/opencode")
                  (lambda (port)
                    (format port "#!~a
export LD_LIBRARY_PATH=~a${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
exec ~a.real \"$@\"~%"
                            bash libpath orig)))
                (chmod (string-append bin "/opencode") #o755)))))))
    (native-inputs (list patchelf))
    (inputs (list bash-minimal `(,gcc "lib") libx11 glibc mesa))
    (home-page "https://opencode.ai")
    (synopsis "CLI AI coding agent")
    (description "The open source coding agent.")
    (license license:expat)))

(define-public glide-browser
  (package
    (name "glide-browser")
    (version "0.1.59a")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/glide-browser/glide/releases/download/" version "/glide.linux-x86_64.tar.xz"))
       (sha256
        (base32 "1yl1cn3qsan7wbv14giw32q4h3bvljbjy0rknfl3r3k579d94hch"))))
    (build-system binary-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     (list #:validate-runpath? #f
           #:install-plan ''(("." "share/glide"))
           #:patchelf-plan ''(("glide" ("libc" "gcc:lib")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-bin
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((bin (string-append #$output "/bin"))
                          (share (string-append #$output "/share/glide"))
                          (exe (string-append bin "/glide")))
                     (mkdir-p bin)
                     (symlink (string-append share "/glide") exe)
                     (wrap-program exe
                       `("LD_LIBRARY_PATH" = (,share ,@(map (lambda (x) (string-append (cdr x) "/lib")) inputs))))))))))
    (inputs (cons*
             `("gcc:lib" ,gcc "lib")
             `("alsa-lib" ,alsa-lib)
             (package-inputs firefox)))
    (home-page "https://glide-browser.app/")
    (synopsis "An extensible and keyboard-focused web browser.")
    (description "Firefox-based keyboard-focused web browser.")
    (license license:mpl2.0)))

(define-public workerd
  (package
   (name "workerd")
   (version "1.20260711.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/cloudflare/workerd/releases/download/v" version "/workerd-linux-64.gz"))
            (sha256
             (base32 "0lzbcw24w5h2hdz09as5iv6j36p35yw780mlis849ddm6azz5mf7"))))
   (build-system binary-build-system)
   (arguments
    `(#:strip-binaries? #f
      #:patchelf-plan '(("workerd-linux-64" ("libc" "gcc:lib")))
      #:install-plan '(("workerd-linux-64" "bin/workerd"))
      #:phases (modify-phases %standard-phases
                 (add-after 'binary-unpack 'chmod
                   (lambda _ (chmod "workerd-linux-64" #o755))))))
   (inputs `(("gcc:lib" ,gcc "lib")))
   (supported-systems '("x86_64-linux"))
   (home-page "https://github.com/cloudflare/workerd")
   (synopsis "The JavaScript / Wasm runtime that powers Cloudflare Workers")
   (description "WASM runtime from Cloudflare")
   (license license:asl2.0)))
