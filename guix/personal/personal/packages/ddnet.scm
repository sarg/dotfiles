(define-module (personal packages ddnet)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph))

(define-public ddnet
  (package
    (name "ddnet")
    (version "20.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ddnet/ddnet")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yxrxmibzm63mqyv5sbsbmxpdwcihnf9n1ckpq5z81hskzwxs0yb"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libs.
        '(for-each delete-file-recursively
                   '("ddnet-libs" "src/engine/external")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DINFORM_UPDATE=OFF"
              "-DAUTOUPDATE=OFF"
              "-DDOWNLOAD_GTEST=OFF"
              "-DPREFER_BUNDLED_LIBS=OFF")
      #:imported-modules `(,@%cmake-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (delete-file ".cargo/config.toml")
              (substitute* "CMakeLists.txt"
                (("(build|test) --locked" _ cmd) cmd)
                (("^.+Cargo\\.lock\n") ""))
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:vendor-dir ".cargo/vendor"
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))))
          (add-after 'unpack 'unbundle-libraries
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; Drop the bundled json and md5 targets...
                (("^set_src\\(DEP_(JSON|MD5)_SRC .*") "")
                (("^add_library\\((json|md5) .*") "")
                (("^list\\(APPEND TARGETS_DEP json md5\\)") "")
                (("\\$<TARGET_OBJECTS:(json|md5)>") "")
                ;; ...and link against the system json-parser instead.
                (("^.+rust_engine_shared\n" all)
                 (string-append "jsonparser\n" all)))
              (substitute* (find-files "src" "\\.(cpp|h)$")
                (("<engine/external/json-parser/json\\.h>")
                 "<json-parser/json.h>"))))
          (add-after 'unpack 'patch-data-dir
            (lambda _
              (substitute* "src/engine/shared/storage.cpp"
                (("\"/usr/share/ddnet\"")
                 (string-append "\"" #$output "/share/ddnet\"")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "run_cxx_tests")))))))
    (native-inputs
     (list
      glslang
      googletest
      pkg-config
      python-minimal
      rust
      `(,rust "cargo")))
    (inputs
     (cons* curl
            ffmpeg
            freetype
            glew
            json-parser
            libnotify
            libogg
            libpng
            libx264
            mesa
            openssl
            opus
            opusfile
            sdl2
            sqlite
            vulkan-headers
            vulkan-loader
            wavpack
            zlib
            (lookup-cargo-inputs 'ddnet)))
    (home-page "https://ddnet.org/")
    (synopsis "DDraceNetwork (DDNet), a cooperative platformer game")
    (description "DDraceNetwork (DDNet) is an actively maintained version of DDRace, a Teeworlds
modification with a unique cooperative gameplay.")
    (license license:zlib)))

;; RUST CRATES

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-cc-1.2.29
  (crate-source "cc" "1.2.29"
                "0qlkaspjmywvjyfqhpv2x4kwrqs6b69zg33wfi2l8fg2im9rj5aw"))

(define rust-clap-4.6.1
  (crate-source "clap" "4.6.1"
                "0lcf88l7vlg796rrqr7wipbbmfa5sgsgx4211b7xmxxv8dz13nqx"))

(define rust-clap-builder-4.6.0
  (crate-source "clap_builder" "4.6.0"
                "17q6np22yxhh5y5v53y4l31ps3hlaz45mvz2n2nicr7n3c056jki"))

(define rust-clap-lex-1.1.0
  (crate-source "clap_lex" "1.1.0"
                "1ycqkpygnlqnndghhcxjb44lzl0nmgsia64x9581030yifxs7m68"))

(define rust-codespan-reporting-0.13.1
  (crate-source "codespan-reporting" "0.13.1"
                "10gnryisncjpfv7wi3jv9mhmrvqz6ksvfzddw3gf99q9k5b1sjdg"))

(define rust-cxx-1.0.194
  (crate-source "cxx" "1.0.194"
                "0zkw4ig6cwss01xfmgnsyp071ji7g49iqd1vv51jyfly64vq8zbl"
                #:snippet '(delete-file-recursively "book")))

(define rust-cxx-build-1.0.194
  (crate-source "cxx-build" "1.0.194"
                "07pwgafaf8qd7ic8f5lbgqgc9zcvm6l7vvvan5vj850a35ynkx5h"))

(define rust-cxxbridge-cmd-1.0.194
  (crate-source "cxxbridge-cmd" "1.0.194"
                "0a63lwv4x36ayxn9fvn7hghjlmf0w4nqy0pd1v2x8y46zacng5fh"))

(define rust-cxxbridge-flags-1.0.194
  (crate-source "cxxbridge-flags" "1.0.194"
                "0fi8rf9076fk53f8gd4a6x1dwffys9d9bqz7mjcavw5lda1llf13"))

(define rust-cxxbridge-macro-1.0.194
  (crate-source "cxxbridge-macro" "1.0.194"
                "1kv3yw1qc9x686kw8z3mmh5ddz98f6v7ghsgzfnjd59bhaswdb76"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-hashbrown-0.17.1
  (crate-source "hashbrown" "0.17.1"
                "0jmqz7i4yl6cm7rbn0i2ffkfrmwi6xkmzkaldr2v8bcsx2v0jngd"))

(define rust-indexmap-2.14.0
  (crate-source "indexmap" "2.14.0"
                "1na9z6f0d5pkjr1lgsni470v98gv2r7c41j8w48skr089x2yjrnl"))

(define rust-libc-0.2.183
  (crate-source "libc" "0.2.183"
                "17c9gyia7rrzf9gsssvk3vq9ca2jp6rh32fsw6ciarpn5djlddmm"))

(define rust-link-cplusplus-1.0.12
  (crate-source "link-cplusplus" "1.0.12"
                "10lcgfp9pnxpihp21s86xnq57vpr97m2k419d8rvkl57m8qcfy3z"))

(define rust-proc-macro2-1.0.103
  (crate-source "proc-macro2" "1.0.103"
                "1s29bz20xl2qk5ffs2mbdqknaj43ri673dz86axdbf47xz25psay"))

(define rust-quote-1.0.41
  (crate-source "quote" "1.0.41"
                "1lg108nb57lwbqlnpsii89cchk6i8pkcvrv88xh1p7a9gdz7c9ff"))

(define rust-scratch-1.0.9
  (crate-source "scratch" "1.0.9"
                "1cj826qggwn482wbfnzij5g9p411qszai0dnfld4qzh93g2jx3yn"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-unicode-ident-1.0.1
  (crate-source "unicode-ident" "1.0.1"
                "131niycgp77aiwvgjdyh47389xfnb7fmlc8ybrxys8v0a0kgxljv"
                #:snippet '(delete-file-recursively "tests")))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-sys-0.61.2
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define-cargo-inputs lookup-cargo-inputs
  (ddnet =>
         (list rust-anstyle-1.0.14
               rust-cc-1.2.29
               rust-clap-4.6.1
               rust-clap-builder-4.6.0
               rust-clap-lex-1.1.0
               rust-codespan-reporting-0.13.1
               rust-cxx-1.0.194
               rust-cxx-build-1.0.194
               rust-cxxbridge-cmd-1.0.194
               rust-cxxbridge-flags-1.0.194
               rust-cxxbridge-macro-1.0.194
               rust-equivalent-1.0.2
               rust-foldhash-0.2.0
               rust-hashbrown-0.17.1
               rust-indexmap-2.14.0
               rust-libc-0.2.183
               rust-link-cplusplus-1.0.12
               rust-proc-macro2-1.0.103
               rust-quote-1.0.41
               rust-scratch-1.0.9
               rust-serde-1.0.228
               rust-serde-core-1.0.228
               rust-serde-derive-1.0.228
               rust-shlex-1.3.0
               rust-strsim-0.11.1
               rust-syn-2.0.106
               rust-termcolor-1.4.1
               rust-unicode-ident-1.0.1
               rust-unicode-width-0.2.2
               rust-winapi-util-0.1.11
               rust-windows-link-0.2.1
               rust-windows-sys-0.61.2)))
