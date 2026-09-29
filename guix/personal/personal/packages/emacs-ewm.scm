(define-module (personal packages emacs-ewm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)

  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define rust-smithay-0.7.0.79bbed5
  (let ((commit "79bbed5e1199090d787115614847a79c76607181"))
    (hidden-package
     (package
       (name "rust-smithay")
       (version (git-version "0.7.0" "1" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/Smithay/smithay")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0dlmm40f34s0fn471fvishhc3c5viilnhmc604v3pr6daj4rqi0d"))))
       (build-system cargo-build-system)
       (arguments
        (list #:skip-build? #t
              #:cargo-package-crates ''("smithay" "smithay-drm-extras")))
       (inputs (lookup-cargo-inputs 'rust-smithay))
       (home-page "https://github.com/Smithay/smithay")
       (synopsis "Smithy for Rust Wayland compositors")
       (description
        "Smithay aims to provide building blocks to create wayland compositors
in Rust.  While not being a full-blown compositor, it'll provide objects and
interfaces implementing common functionalities that pretty much any compositor
will need, in a generic fashion.

It supports the @code{wayland}, @code{wayland-protocols}, and some external
extensions, such as @code{wlr-protocols} and @code{plasma-wayland-protocols}.")
       (license license:expat)))))

(define rust-elisp
  (package
    (name "rust-elisp")
    (properties '((commit . "44f20d3ae754bbefdbeec184c5490a99935166a0")))
    (version (git-version "0.0.1" "1" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/elisp")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "187p9xq4213p09qjarhzgynpf0lf35i4c644isbd2mk8bdrc0nkg"))))
    (build-system cargo-build-system)
    (arguments
     (list #:skip-build? #t
           #:cargo-package-crates ''("elisp" "elisp-emacs")))
    (inputs (lookup-cargo-inputs 'rust-elisp))
    (home-page "https://codeberg.org/ezemtsov/elisp")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public emacs-ewm
  (package
    (name "emacs-ewm")
    (properties '((commit . "8453d28933681f8d2d6102106977727cfbbe7153")))
    (version (git-version "0.1.0" "26" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/ezemtsov/ewm")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16xa86kz580qldgnmqm0dnyk4nbp68jbl96qwxf6hgdmpz02hji0"))))
    (build-system cargo-build-system)
    (arguments
     (list #:install-source? #f
           #:modules '((guix build cargo-build-system)
                       ((guix build emacs-build-system) #:prefix emacs:)
                       (guix build emacs-utils)
                       (guix build utils))
           #:imported-modules `(,@%cargo-build-system-modules
                                (guix build emacs-utils)
                                (guix build emacs-build-system))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'build-lisp
                 (lambda args
                   (with-directory-excursion "lisp"
                     (substitute* "ewm.el"
                       (("\\(getenv \"EWM_MODULE_PATH\"\\)")
                        (string-append "\"" #$output "/lib/libewm_core.so\"")))

                     (for-each
                      (lambda (phase)
                        (apply (cdr phase) args))
                      (modify-phases emacs:%standard-phases
                        (delete 'unpack))))))
               
               (add-after 'build-lisp 'fix-deps
                 (lambda _
                   (chdir "compositor")
                   (delete-file "Cargo.lock")
                   (substitute* "Cargo.toml"
                     (("^elisp =.*") "elisp = { version = \"*\", features = [\"serde\"]}\n")
                     (("^elisp-emacs =.*") "elisp-emacs = { version = \"*\" }\n")
                     (("^rev =.*") "version = \"*\"\n")
                     (("^git = .*") ""))))

               (replace 'install
                 (lambda _
                   (install-file "target/release/libewm_core.so"
                                 (string-append #$output "/lib")))))))
    (native-inputs (list emacs-minimal pkg-config))
    (inputs (cons*
             dbus
             libdisplay-info
             libinput-minimal
             libseat
             libxkbcommon
             mesa
             pipewire
             wayland
             glib
             libx11
             libxcursor
             libxrandr
             libxi
             libdrm
             (lookup-cargo-inputs 'emacs-ewm)))
    (home-page "https://codeberg.org/ezemtsov/ewm")
    (synopsis "Emacs Wayland Manager")
    (description "Emacs Wayland Manager - Wayland compositor")
    (license license:gpl3+)))

;; RUST CRATES

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-aliasable-0.1.3
  (crate-source "aliasable" "0.1.3"
                "1z8548zdjlm4ps1k0d7x68lfdyji02crwcc9rw3q3bb106f643r5"))

(define rust-aligned-0.4.3
  (crate-source "aligned" "0.4.3"
                "1186lhb3gb4x6spzw7ff0zcraa8cr9zqk4ldpm5g1vb2ijc0higf"))

(define rust-aligned-vec-0.6.4
  (crate-source "aligned-vec" "0.6.4"
                "16vnf78hvfix5cwzd5xs5a2g6afmgb4h7n6yfsc36bv0r22072fw"))

(define rust-android-activity-0.6.0
  (crate-source "android-activity" "0.6.0"
                "0inh88x8x2fh62jg739s9hwyvdh8i920qf0qw7bhr802j9c7hsgg"))

(define rust-android-properties-0.2.2
  (crate-source "android-properties" "0.2.2"
                "016slvg269c0y120p9qd8vdfqa2jbw4j0g18gfw6p3ain44v4zpw"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-annotate-snippets-0.11.5
  (crate-source "annotate-snippets" "0.11.5"
                "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.101
  (crate-source "anyhow" "1.0.101"
                "1skmg90fnjnlgs3vl7bksw7036d3rqwqj20n2fxd2ppg67p0y3jz"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-anyhow-1.0.104
  (crate-source "anyhow" "1.0.104"
                "0w34jjcm02p5g9kvsjr1dvpw0zs2fi7igi6nr414fkm5gz85w2ik"))

(define rust-appendlist-1.4.0
  (crate-source "appendlist" "1.4.0"
                "1lnbl7mc7capcqj1z1ylxvm4h492sb9sr8pzww3q6lrhrmrxqjg1"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arg-enum-proc-macro-0.3.4
  (crate-source "arg_enum_proc_macro" "0.3.4"
                "1sjdfd5a8j6r99cf0bpqrd6b160x9vz97y5rysycsjda358jms8a"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-as-raw-xcb-connection-1.0.1
  (crate-source "as-raw-xcb-connection" "1.0.1"
                "0sqgpz2ymv5yx76r5j2npjq2x5qvvqnw0vrs35cyv30p3pfp2m8p"))

(define rust-as-slice-0.2.1
  (crate-source "as-slice" "0.2.1"
                "05j52y1ws8kir5zjxnl48ann0if79sb56p9nm76hvma01r7nnssi"))

(define rust-ash-0.38.0+1.3.281
  (crate-source "ash" "0.38.0+1.3.281"
                "0vx4yf689v1rc680jvy8bnysx5sgd8f33wnp2vqaizh0v0v4kd0b"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-channel-2.5.0
  (crate-source "async-channel" "2.5.0"
                "1ljq24ig8lgs2555myrrjighycpx2mbjgrm3q7lpa6rdsmnxjklj"))

(define rust-async-executor-1.13.3
  (crate-source "async-executor" "1.13.3"
                "1f3za9v8wkqzv6rz69g0qzvdcmghwbixijwzldwjm9w3zph00z29"))

(define rust-async-io-2.6.0
  (crate-source "async-io" "2.6.0"
                "1z16s18bm4jxlmp6rif38mvn55442yd3wjvdfhvx4hkgxf7qlss5"))

(define rust-async-lock-3.4.2
  (crate-source "async-lock" "3.4.2"
                "04c3xrrdrfrvh9v0ajxrangpy38qi76qq268zslphnxxjqjpy3r9"))

(define rust-async-process-2.5.0
  (crate-source "async-process" "2.5.0"
                "0xfswxmng6835hjlfhv7k0jrfp7czqxpfj6y2s5dsp05q0g94l7w"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-signal-0.2.13
  (crate-source "async-signal" "0.2.13"
                "0k66mpb3xp86hj4vxs7w40v7qz2jfbblrm9ddc5mglwwynxp1h23"))

(define rust-async-task-4.7.1
  (crate-source "async-task" "4.7.1"
                "1pp3avr4ri2nbh7s6y9ws0397nkx1zymmcr14sq761ljarh3axcb"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-autocfg-1.5.1
  (crate-source "autocfg" "1.5.1"
                "0lqasy5i30flcgih1b50kvsk6z32g09r1q4ql7q81pj6228jy0zj"))

(define rust-av-scenechange-0.14.1
  (crate-source "av-scenechange" "0.14.1"
                "1543y7riwcy4mmsgcalxcm3bnb41hvwiqiz774nbj68fq9vischg"))

(define rust-av1-grain-0.2.5
  (crate-source "av1-grain" "0.2.5"
                "1y3p43i5xncbny0pfh8kw09am3l3mgyg82ln65r3f434443xpzcc"))

(define rust-avif-serialize-0.8.8
  (crate-source "avif-serialize" "0.8.8"
                "0gd5hr9vd2rkf9gn60f39rham6lzn8a4cdy0p57ihrxx0zq84l1p"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bindgen-0.70.1
  (crate-source "bindgen" "0.70.1"
                "0vyf0jp6apcy9kjyz4s8vldj0xqycnbzb6zv3skkwiqdi3nqz7gl"))

(define rust-bindgen-0.72.1
  (crate-source "bindgen" "0.72.1"
                "15bq73y3wd3x3vxh3z3g72hy08zs8rxg1f0i1xsrrd6g16spcdwr"))

(define rust-bit-field-0.10.3
  (crate-source "bit_field" "0.10.3"
                "1ikhbph4ap4w692c33r8bbv6yd2qxm1q3f64845grp1s6b3l0jqy"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitflags-2.11.0
  (crate-source "bitflags" "2.11.0"
                "1bwjibwry5nfwsfm9kjg2dqx5n5nja9xymwbfl6svnn8jsz6ff44"))

(define rust-bitflags-2.13.2
  (crate-source "bitflags" "2.13.2"
                "01hbgjwvid66850fzi76mvn5f2bqycx6sf165ng1kfjqq9bl1v9x"))

(define rust-bitstream-io-4.9.0
  (crate-source "bitstream-io" "4.9.0"
                "0mqpwqy8cqnaqr4wkpk7qy7fjp4x6vxaf8z2hprbvimj3nfvvm30"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-blocking-1.6.2
  (crate-source "blocking" "1.6.2"
                "08bz3f9agqlp3102snkvsll6wc9ag7x5m1xy45ak2rv9pq18sgz8"))

(define rust-borsh-1.8.1
  (crate-source "borsh" "1.8.1"
                "02p2lhz49944wc6j5dzr42y1mkrv0znbicg3cl61b9bbda25sg2m"))

(define rust-built-0.8.0
  (crate-source "built" "0.8.0"
                "0r5f08lpjsr6j5ajkbmd0ymfmajpq8ddbfvi8ji8rx48y88qzbgl"))

(define rust-bumpalo-3.19.1
  (crate-source "bumpalo" "3.19.1"
                "044555i277xcinmqs7nnv8n5y4fqfi4l4lp1mp3i30vsidrxrnax"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-bytemuck-1.25.0
  (crate-source "bytemuck" "1.25.0"
                "1v1z32igg9zq49phb3fra0ax5r2inf3aw473vldnm886sx5vdvy8"))

(define rust-bytemuck-derive-1.10.2
  (crate-source "bytemuck_derive" "1.10.2"
                "1zvmjmw1sdmx9znzm4dpbb2yvz9vyim8w6gp4z256l46qqdvvazr"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-byteorder-lite-0.1.0
  (crate-source "byteorder-lite" "0.1.0"
                "15alafmz4b9az56z6x7glcbcb6a8bfgyd109qc3bvx07zx4fj7wg"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-calloop-0.13.0
  (crate-source "calloop" "0.13.0"
                "1v5zgidnhsyml403rzr7vm99f8q6r5bxq5gxyiqkr8lcapwa57dr"))

(define rust-calloop-0.14.3
  (crate-source "calloop" "0.14.3"
                "17ih3c840cqksv9ms7i2ynnkiabpvqvpxakbr3922imxd09nx7yb"))

(define rust-calloop-0.14.4
  (crate-source "calloop" "0.14.4"
                "1xsd8xk53v9zbvhjy7ynf4gya9s4rvvh8jqx9psi1b2v6rw9kgsd"))

(define rust-calloop-wayland-source-0.3.0
  (crate-source "calloop-wayland-source" "0.3.0"
                "086x5mq16prrcwd9k6bw9an0sp8bj9l5daz4ziz5z4snf2c6m9lm"))

(define rust-calloop-wayland-source-0.4.1
  (crate-source "calloop-wayland-source" "0.4.1"
                "1yi1c23naqhd8m94q3v366s4cak8l50zy7ldrkqfn0hajkqgr3hk"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-cc-1.2.55
  (crate-source "cc" "1.2.55"
                "0adx36r84c7rscv853a71nd3d5gsb1jf438gnl4syd5fah4nmcj7"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cesu8-1.1.0
  (crate-source "cesu8" "1.1.0"
                "0g6q58wa7khxrxcxgnqyi9s1z2cjywwwd3hzr5c55wskhx6s0hvd"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.2
  (crate-source "cfg_aliases" "0.2.2"
                "09rm3dv28gbsal7w6q76lg2nfyn8wp789ska9b8vr1w750xfhygh"))

(define rust-cfg-expr-0.20.6
  (crate-source "cfg-expr" "0.20.6"
                "0smbxbd39s2kpmz6r9yg4xmh0wx5d1in6amf49rpr0m6l6szbkkq"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-chacha20-0.10.2
  (crate-source "chacha20" "0.10.2"
                "01hvvbgdmqkcgs2s4f12s9wa5h2gbq05rqvypv61azlwd55mxhv5"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-clang-sys-1.8.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

(define rust-color-quant-1.1.0
  (crate-source "color_quant" "1.1.0"
                "12q1n427h2bbmmm1mnglr57jaz2dj9apk0plcxw7nwqiai7qjyrx"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-combine-4.6.7
  (crate-source "combine" "4.6.7"
                "1z8rh8wp59gf8k23ar010phgs0wgf5i8cx4fg01gwcnzfn5k0nms"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-console-0.15.11
  (crate-source "console" "0.15.11"
                "1n5gmsjk6isbnw6qss043377kln20lfwlmdk3vswpwpr21dwnk05"))

(define rust-container-of-0.5.1
  (crate-source "container_of" "0.5.1"
                "0as7g6gspvdbp4vl1a1834pzh481x9jp4clfgyl6c7vnhvmvpxc9"))

(define rust-cookie-factory-0.3.3
  (crate-source "cookie-factory" "0.3.3"
                "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq"))

(define rust-core2-0.4.0
  (crate-source "core2" "0.4.0"
                "01f5xv0kf3ds3xm7byg78hycbanb8zlpvsfv4j47y46n3bpsg6xl"))

(define rust-cpufeatures-0.3.1
  (crate-source "cpufeatures" "0.3.1"
                "0rkm1l35jy1z1yfg723fddsxc7vr0gc1hhfvc138hnqiwc58p8jw"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-criterion-0.5.1
  (crate-source "criterion" "0.5.1"
                "0bv9ipygam3z8kk6k771gh9zi0j0lb9ir0xi1pc075ljg80jvcgj"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-ctor-0.1.26
  (crate-source "ctor" "0.1.26"
                "15m0wqhv12p25xkxz5dxvg23r7a6bkh7p8zi1cdhgswjhdl028vd"))

(define rust-cursor-icon-1.2.0
  (crate-source "cursor-icon" "1.2.0"
                "0bvkw7ak1mqwcpkgd9lh7n00hcvlh87jfl7188f231nz6zfy2ypj"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-dispatch2-0.3.1
  (crate-source "dispatch2" "0.3.1"
                "0f5xmnbzpaz1g80m27kd804p75nswh0ikb6wvqh4ba3x9rz3c3hy"))

(define rust-displaydoc-0.2.7
  (crate-source "displaydoc" "0.2.7"
                "1a42mwpgpwcqq2qqgkcc630wvsc2p2dkmgacjnclginwfz9js8y6"))

(define rust-dlib-0.5.3
  (crate-source "dlib" "0.5.3"
                "0jpr4smrwrv8xj70mz4ixnbc6ljm82f12z2mz1hv89056y3wv3mb"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.2
  (crate-source "dpi" "0.1.2"
                "0xhsvzgjvdch2fwmfc9vkb708b0q59b6imypyjlgbiigyb74rcfq"))

(define rust-drm-0.14.1
  (crate-source "drm" "0.14.1"
                "0vvmj9n0wslrbw3rinpzlfyhwwgr02gqspy1al5gfh99dif8rg40"))

(define rust-drm-ffi-0.9.1
  (crate-source "drm-ffi" "0.9.1"
                "147n13dnkr4kzdj4662dqgbjfvnnw14yhmf2vq2q2kmc6adiraai"))

(define rust-drm-fourcc-2.2.0
  (crate-source "drm-fourcc" "2.2.0"
                "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))

(define rust-drm-sys-0.8.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "drm-sys" "0.8.1"
                "1y59h9x5yn9p36f9bqjvw76kx75yqfin1w6gzigiznb620vf3j7c"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-emacs-0.21.0
  (crate-source "emacs" "0.21.0"
                "1qdqn0bk76fq24zqlmapj2ancm0m1hwalacsxmk146gmv1mny23w"))

(define rust-emacs-macros-0.17.0
  (crate-source "emacs-macros" "0.17.0"
                "0qg1dcn5acbirq617qq2fgg9adswif2dnr292s3qnq62wzgnyrb9"))

(define rust-emacs-module-0.21.0
  (crate-source "emacs_module" "0.21.0"
                "0ykpk4fxpz193fykchb6xs8l5lbghzl6panwy79yh74hvgxj1018"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.1
  (crate-source "endi" "1.1.1"
                "16a0076dx41vgrzzimm9clcym77h732czqjiajanmzvd1i1y5dv6"))

(define rust-enumflags2-0.7.12
  (crate-source "enumflags2" "0.7.12"
                "1vzcskg4dca2jiflsfx1p9yw1fvgzcakcs7cpip0agl51ilgf9qh"))

(define rust-enumflags2-derive-0.7.12
  (crate-source "enumflags2_derive" "0.7.12"
                "09rqffacafl1b83ir55hrah9gza0x7pzjn6lr6jm76fzix6qmiv7"))

(define rust-equator-0.4.2
  (crate-source "equator" "0.4.2"
                "1z760z5r0haxjyakbqxvswrz9mq7c29arrivgq8y1zldhc9v44a7"))

(define rust-equator-macro-0.4.2
  (crate-source "equator-macro" "0.4.2"
                "1cqzx3cqn9rxln3a607xr54wippzff56zs5chqdf3z2bnks3rwj4"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-event-listener-5.4.1
  (crate-source "event-listener" "5.4.1"
                "1asnp3agbr8shcl001yd935m167ammyi8hnvl0q1ycajryn6cfz1"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-exr-1.74.0
  (crate-source "exr" "1.74.0"
                "1gk3cc2qkfm0jqw4v1d7g4c356k9iz583bq17iiwp8kalm1y0023"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fastrand-2.5.0
  (crate-source "fastrand" "2.5.0"
                "08q2r30y62winysimnlpbvw9kiwn0rmdlidqlmzd6z90mv764z6s"))

(define rust-fax-0.2.6
  (crate-source "fax" "0.2.6"
                "1ax0jmvsszxd03hj6ga1kyl7gaqcfw0akg2wf0q6gk9pizaffpgh"))

(define rust-fax-derive-0.2.0
  (crate-source "fax_derive" "0.2.0"
                "0zap434zz4xvi5rnysmwzzivig593b4ng15vwzwl7js2nw7s3b50"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-float-cmp-0.9.0
  (crate-source "float-cmp" "0.9.0"
                "1i799ksbq7fj9rm9m82g1yqgm6xi3jnrmylddmqknmksajylpplq"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fps-ticker-1.0.0
  (crate-source "fps_ticker" "1.0.0"
                "06cj5c5rk5grm2ajh4sabcppxr1h57gxfqacvi5psxb9zw2lj5py"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-2.6.1
  (crate-source "futures-lite" "2.6.1"
                "1ba4dg26sc168vf60b1a23dv1d8rcf3v3ykz2psb7q70kxh113pp"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-gbm-0.18.0
  (crate-source "gbm" "0.18.0"
                "0skyaj51xlazaa24jdkxxi2g6pnw834k3yqlf2ly999wincjx1ff"))

(define rust-gbm-sys-0.4.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "gbm-sys" "0.4.0"
                "0vzp28ip4w74p05ygs4p9m7sspggn2zvcykbpyv8ypbqrhm5yfn1"))

(define rust-generator-0.8.8
  (crate-source "generator" "0.8.8"
                "1ybcxxz9vdh7nyh9q5654zv5q790b63a83w0zrv0r8id2pj4mw2j"))

(define rust-gethostname-1.1.0
  (crate-source "gethostname" "1.1.0"
                "1n6bj9gh503ggjblfjcai96gmxynxsrykaynljlrfdra34q95m0v"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-getrandom-0.4.3
  (crate-source "getrandom" "0.4.3"
                "16b0202fkdwz3p2cyll82dv24ljbn0wiyy829v4lwbkbflyqh3ih"))

(define rust-gif-0.14.1
  (crate-source "gif" "0.14.1"
                "0pn3ldqjk0ng1vbc3r3zqqrnjkn6s3f3ndk96lhhrn0q82l2ppzm"))

(define rust-gio-0.20.12
  (crate-source "gio" "0.20.12"
                "0cdq5116cwdgs0xkdp1v146yhcqilxlpgvkncc7xbf5nwxvf49wf"))

(define rust-gio-sys-0.20.10
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "gio-sys" "0.20.10"
                "10vc6gqhz5crnrh040rv6r5nm09njky2r9d9ms29xj3gwnkr67jj"))

(define rust-gl-generator-0.14.0
  (crate-source "gl_generator" "0.14.0"
                "0k8j1hmfnff312gy7x1aqjzcm8zxid7ij7dlb8prljib7b1dz58s"))

(define rust-glam-0.33.11
  (crate-source "glam" "0.33.11"
                "0kzjf8xfvksrz52kbhd1j3ih36abd8a3wa6g3mfn8cyil4vyid1m"))

(define rust-glib-0.20.12
  (crate-source "glib" "0.20.12"
                "10ynn8aiabbzrsgdswmqpr47sapfkbfn5rfxsy26swflabivdi7z"))

(define rust-glib-macros-0.20.12
  (crate-source "glib-macros" "0.20.12"
                "0ibi9vbpbw9vvl9ax60kxq07d7a21k0jj5lva8zmliq95zv4l278"))

(define rust-glib-sys-0.20.10
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "glib-sys" "0.20.10"
                "05f29ky5dnvy8vp5rdld5f8r2lgr5w7dxqr7p27km016s4g9xdwa"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-glow-0.16.0
  (crate-source "glow" "0.16.0"
                "022z12nlyfpy36fvp2szq792xix1xbgkznpmicf1c404sxhfmrf5"))

(define rust-gobject-sys-0.20.10
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "gobject-sys" "0.20.10"
                "1niyqv22b2c38ks33i4isas4v83d3w7jx3xzzly9x63kpfacm6pc"))

(define rust-half-2.7.1
  (crate-source "half" "2.7.1"
                "0jyq42xfa6sghc397mx84av7fayd4xfxr4jahsqv90lmjr5xi8kf"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.3.9
  (crate-source "hermit-abi" "0.3.9"
                "092hxjbjnq5fmz66grd9plxd0sh6ssg5fhgwwwqbrzgzkjwdycfj"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-icu-collections-2.2.0
  (crate-source "icu_collections" "2.2.0"
                "070r7xd0pynm0hnc1v2jzlbxka6wf50f81wybf9xg0y82v6x3119"))

(define rust-icu-locale-core-2.2.0
  (crate-source "icu_locale_core" "2.2.0"
                "0a9cmin5w1x3bg941dlmgszn33qgq428k7qiqn5did72ndi9n8cj"))

(define rust-icu-normalizer-2.2.0
  (crate-source "icu_normalizer" "2.2.0"
                "1d7krxr0xpc4x9635k1100a24nh0nrc59n65j6yk6gbfkplmwvn5"))

(define rust-icu-normalizer-data-2.2.0
  (crate-source "icu_normalizer_data" "2.2.0"
                "0f5d5d5fhhr9937m2z6z38fzh6agf14z24kwlr6lyczafypf0fys"))

(define rust-icu-properties-2.2.0
  (crate-source "icu_properties" "2.2.0"
                "1pkh3s837808cbwxvfagwc28cvwrz2d9h5rl02jwrhm51ryvdqxy"))

(define rust-icu-properties-data-2.2.0
  (crate-source "icu_properties_data" "2.2.0"
                "052awny0qwkbcbpd5jg2cd7vl5ry26pq4hz1nfsgf10c3qhbnawf"))

(define rust-icu-provider-2.2.0
  (crate-source "icu_provider" "2.2.0"
                "08dl8pxbwr8zsz4c5vphqb7xw0hykkznwi4rw7bk6pwb3krlr70k"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.2
  (crate-source "idna_adapter" "1.2.2"
                "0557p76l8hj35r9zn1yv7c6x1c0qbrsffmg80n0yy8361ly3fs6b"))

(define rust-image-0.25.9
  (crate-source "image" "0.25.9"
                "06lwa4ag3zcmjzivl356q0qhgxxqpkp7qwda7x0mjrkq21n6ql76"))

(define rust-image-webp-0.2.4
  (crate-source "image-webp" "0.2.4"
                "1hz814csyi9283vinzlkix6qpnd6hs3fkw7xl6z2zgm4w7rrypjj"))

(define rust-imgref-1.12.0
  (crate-source "imgref" "1.12.0"
                "1j3iwdal9mdkmyrsms3lz4n1bxxxjxss2jvbmh662fns63fcxig7"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-input-0.10.0
  (crate-source "input" "0.10.0"
                "17cmlwa5z6z3x47r7m78vmh8f7rmv2sncc53cdvk2waxlr2k6ygr"))

(define rust-input-sys-1.19.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "input-sys" "1.19.0"
                "1rqrrglhgyyiy7xh8jvf9jwh2dyk2g7l5rmjaazrbg82iryy1vin"))

(define rust-insta-1.46.3
  (crate-source "insta" "1.46.3"
                "1r0mc4sjayarbl5cbizk4wa0hwwakcwj836f6k5ww73zgk4bhbg8"))

(define rust-instant-0.1.13
  (crate-source "instant" "0.1.13"
                "08h27kzvb5jw74mh0ajv0nv9ggwvgqm8ynjsn2sa9jsks4cjh970"))

(define rust-interpolate-name-0.2.4
  (crate-source "interpolate_name" "0.2.4"
                "0q7s5mrfkx4p56dl8q9zq71y1ysdj4shh6f28qf9gly35l21jj63"))

(define rust-io-lifetimes-1.0.11
  (crate-source "io-lifetimes" "1.0.11"
                "1hph5lz4wd3drnn6saakwxr497liznpfnv70via6s0v8x6pbkrza"))

(define rust-is-terminal-0.4.17
  (crate-source "is-terminal" "0.4.17"
                "0ilfr9n31m0k6fsm3gvfrqaa62kbzkjqpwcd9mc46klfig1w2h1n"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jni-0.21.1
  (crate-source "jni" "0.21.1"
                "15wczfkr2r45slsljby12ymf2hij8wi5b104ghck9byjnwmsm1qs"))

(define rust-jni-sys-0.3.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "jni-sys" "0.3.0"
                "0c01zb9ygvwg9wdx2fii2d39myzprnpqqhy7yizxvjqp5p04pbwf"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-js-sys-0.3.90
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.90"
                "19m5qg024y5xanjrq5c6m1sx69nnzqw7ychnbgnx9xmka1j6zp0l"))

(define rust-keyboard-types-0.8.3
  (crate-source "keyboard-types" "0.8.3"
                "13dj6jsk4rr4kqghlxyr3ql7ajfrg65awc1h4c21mris80xqbghg"))

(define rust-khronos-api-3.1.0
  (crate-source "khronos_api" "3.1.0"
                "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-lebe-0.5.3
  (crate-source "lebe" "0.5.3"
                "1f459clndzzm35nyd15vj5dlasyagfasp7hcgl6lh2b658rs6ybs"))

(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

(define rust-libc-0.2.189
  (crate-source "libc" "0.2.189"
                "1whjfs375vlng2q6yrbzs73cvp5lm3w1n2gfqajb2vgf7zg3xbry"))

(define rust-libdisplay-info-0.3.0
  (crate-source "libdisplay-info" "0.3.0"
                "0nf3c4rpdhgpr8g7dn2wrjyzwl45vz5sq1sg64gz67rqnbdrdzar"))

(define rust-libdisplay-info-derive-0.1.1
  (crate-source "libdisplay-info-derive" "0.1.1"
                "162ahw5kry0d7yf50b62dhw18s6c9bkdjim4409fj6aqrw8cghld"))

(define rust-libdisplay-info-sys-0.3.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libdisplay-info-sys" "0.3.0"
                "07xmkc2aqcdn6d58321y87rd3gzdr4nx3ncm1mmrr7w1p1ahsn96"))

(define rust-libfuzzer-sys-0.4.12
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libfuzzer-sys" "0.4.12"
                "13ghagfsynmqda1pkpalila6kf0llqxh3214ynzi5knqgldnhapi"))

(define rust-libloading-0.7.4
  (crate-source "libloading" "0.7.4"
                "17wbccnjvhjd9ibh019xcd8kjvqws8lqgq86lqkpbgig7gyq0wxn"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libredox-0.1.25
  (crate-source "libredox" "0.1.25"
                "078pc91l1c1w9hbzi7m4ph5qjnk4i2zgsqhg4hx80yh7yv591zv1"))

(define rust-libseat-0.2.4
  (crate-source "libseat" "0.2.4"
                "0cggn682xklm5h7i8bbjc48wjpys9wz2y8xa7ywgyrh3dsdwcmk6"))

(define rust-libseat-sys-0.2.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libseat-sys" "0.2.0"
                "1yvx15lx8qj3xycdx4ddzs681ayhg5vpdvgzsfl64pxy93x89978"))

(define rust-libspa-0.10.1
  (crate-source "libspa" "0.10.1"
                "0h2c1f7s99ylki3gm7vgd7bs3403cg20azwb72fwr7cqwwkp8bw8"))

(define rust-libspa-sys-0.10.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libspa-sys" "0.10.1"
                "090r57qrpz16mis8qbhi6fbzsh9b0cj2c0j485g0vvb3myyifviv"))

(define rust-libudev-sys-0.1.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.8.3
  (crate-source "litemap" "0.8.3"
                "1bpgpj87560hmckh3875fbahpmfxbk4g8pzns84h3ykf3nfx3na7"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-loop9-0.1.5
  (crate-source "loop9" "0.1.5"
                "0qphc1c0cbbx43pwm6isnwzwbg6nsxjh7jah04n1sg5h4p0qgbhg"))

(define rust-lz4-flex-0.10.0
  (crate-source "lz4_flex" "0.10.0"
                "10sgbj93sagbl0ngzqvnlkldzbfz5vnzr7fry8sgssy299cp534b"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memmap2-0.8.0
  (crate-source "memmap2" "0.8.0"
                "1vf3djv9s917fbvw5vclllpl22g12iph6cz11gn57ndhxwya19a3"))

(define rust-memmap2-0.9.10
  (crate-source "memmap2" "0.9.10"
                "1qz0n4ch68pz2mp07sdwnk27imdjjqy6aqir3hp9j4g0iw19hh3i"))

(define rust-memmap2-0.9.9
  (crate-source "memmap2" "0.9.9"
                "146lfx0mpib44wvws6hibahm4h2w867bzwsc6zhmi9p0l3j36hbl"))

(define rust-memoffset-0.6.5
  (crate-source "memoffset" "0.6.5"
                "1kkrzll58a3ayn5zdyy9i1f1v3mx0xgl29x0chq614zazba638ss"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-moxcms-0.7.11
  (crate-source "moxcms" "0.7.11"
                "15qa5znj029i7677l0hdv0lwmjggrg920bhjgs3cjvydb72mg5dc"))

(define rust-ndk-0.9.0
  (crate-source "ndk" "0.9.0"
                "1m32zpmi5w1pf3j47k6k5fw395dc7aj8d0mdpsv53lqkprxjxx63"))

(define rust-ndk-context-0.1.1
  (crate-source "ndk-context" "0.1.1"
                "12sai3dqsblsvfd1l1zab0z6xsnlha3xsfl7kagdnmj3an3jvc17"))

(define rust-ndk-sys-0.6.0+11769913
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "ndk-sys" "0.6.0+11769913"
                "0wx8r6pji20if4xs04g73gxl98nmjrfc73z0v6w1ypv6a4qdlv7f"))

(define rust-new-debug-unreachable-1.0.6
  (crate-source "new_debug_unreachable" "1.0.6"
                "11phpf1mjxq6khk91yzcbd3ympm78m3ivl7xg6lg2c0lf66fy3k5"))

(define rust-nix-0.27.1
  (crate-source "nix" "0.27.1"
                "0ly0kkmij5f0sqz35lx9czlbk6zpihb7yh1bsy4irzwfd2f4xc1f"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-enum-0.7.5
  (crate-source "num_enum" "0.7.5"
                "0k25hagf3xfgmj4j1zmvja1d6844jrmpginxpd3vhmxd41z7l85i"))

(define rust-num-enum-derive-0.7.5
  (crate-source "num_enum_derive" "0.7.5"
                "1mx4dgza8b9g16baybc00gg06jn4cf17h45p0fr3qx5nw5fkccpz"))

(define rust-num-integer-0.1.46
  (crate-source "num-integer" "0.1.46"
                "13w5g54a9184cqlbsq80rnxw4jj4s0d8wv75jsq5r2lms8gncsbr"))

(define rust-num-rational-0.4.2
  (crate-source "num-rational" "0.4.2"
                "093qndy02817vpgcqjnj139im3jl7vkq4h68kykdqqh577d18ggq"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-objc2-0.6.4
  (crate-source "objc2" "0.6.4"
                "17x8qpl512frscfqbmgjr20kg3y4r0xdqxphja17dz5f0znsh4is"))

(define rust-objc2-app-kit-0.3.2
  (crate-source "objc2-app-kit" "0.3.2"
                "132ijwni8lsi8phq7wnmialkxp46zx998fns3zq5np0ya1mr77nl"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-core-graphics-0.3.2
  (crate-source "objc2-core-graphics" "0.3.2"
                "01x8413pxq0m5rwidlaczni8v5cz9dc3xqzq8l9zlpl9cv8cj8p0"))

(define rust-objc2-core-video-0.3.2
  (crate-source "objc2-core-video" "0.3.2"
                "19j1a7f863gh30nq03w70x1js3f3vdg3wp4azllky8vkvzqwl9fl"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.2
  (crate-source "objc2-foundation" "0.3.2"
                "0wijkxzzvw2xkzssds3fj8279cbykz2rz9agxf6qh7y2agpsvq73"))

(define rust-objc2-ui-kit-0.3.2
  (crate-source "objc2-ui-kit" "0.3.2"
                "08mbgqg8pffclyxpz2lr8r1fv8wn2i4m1k6bk1s5fvy06f766zfq"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-orbclient-0.4.6
  (crate-source "orbclient" "0.4.6"
                "0sry8hmkxv87l056qhkgmnq2zzvgjdlx7pqjg4yj107i72kl1w81"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-pastey-0.1.1
  (crate-source "pastey" "0.1.1"
                "1v389jkifv757903flrrps67dvc6q6giwlyx3xi33hcfjmgjxyrm"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pin-project-1.1.10
  (crate-source "pin-project" "1.1.10"
                "12kadbnfm1f43cyadw9gsbyln1cy7vj764wz5c8wxaiza3filzv7"))

(define rust-pin-project-internal-1.1.10
  (crate-source "pin-project-internal" "1.1.10"
                "0qgqzfl0f4lzaz7yl5llhbg97g68r15kljzihaw9wm64z17qx4bf"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-piper-0.2.4
  (crate-source "piper" "0.2.4"
                "0rn0mjjm0cwagdkay77wgmz3sqf8fqmv9d9czm79mvr2yj8c9j4n"))

(define rust-pipewire-0.10.1
  (crate-source "pipewire" "0.10.1"
                "0p1dlfx7jfql21ahpbswiqrxpv75fnm4v98yizb5jng2qj211rxx"))

(define rust-pipewire-sys-0.10.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "pipewire-sys" "0.10.1"
                "1q1wsrfra5k3n8fkyby9hb8aa1l9kbp95011acwbkrb37vsm7rlw"))

(define rust-pixman-0.2.1
  (crate-source "pixman" "0.2.1"
                "1pqybqb7rmd58yr9xvmd8iix30znw5w71cq2wnlc16n1jva1g8nf"))

(define rust-pixman-sys-0.1.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "pixman-sys" "0.1.0"
                "1nja8kc7zs1w4lhllvsgssa0b07n4cgwb0zyvqapj7g8i4z4i851"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-plotters-0.3.7
  (crate-source "plotters" "0.3.7"
                "0ixpy9svpmr2rkzkxvvdpysjjky4gw104d73n7pi2jbs7m06zsss"))

(define rust-plotters-backend-0.3.7
  (crate-source "plotters-backend" "0.3.7"
                "0ahpliim4hrrf7d4ispc2hwr7rzkn6d6nf7lyyrid2lm28yf2hnz"))

(define rust-plotters-svg-0.3.7
  (crate-source "plotters-svg" "0.3.7"
                "0w56sxaa2crpasa1zj0bhxzihlapqfkncggavyngg0w86anf5fji"))

(define rust-png-0.18.1
  (crate-source "png" "0.18.1"
                "0qca282xp8a6d7mikxrwji3f52mjn4vnqxz2v9iz5adj665rnxk0"))

(define rust-polling-3.11.0
  (crate-source "polling" "3.11.0"
                "0622qfbxi3gb0ly2c99n3xawp878fkrd1sl83hjdhisx11cly3jx"))

(define rust-portable-atomic-1.15.0
  (crate-source "portable-atomic" "1.15.0"
                "11csag858ndk5w4yz17h91vy53ynh67r2903gwwdn2cnilzbdj05"))

(define rust-potential-utf-0.1.6
  (crate-source "potential_utf" "0.1.6"
                "0qbndl2fpphq7mph41m11vaixs05xrh1s451wxlgap4fdnybjgnq"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proc-macro2-1.0.107
  (crate-source "proc-macro2" "1.0.107"
                "1nb6ly8kp65f724kj73ippc7lvydss24sm2vagk6qpklpg4pwplq"))

(define rust-profiling-1.0.17
  (crate-source "profiling" "1.0.17"
                "0wqp6i1bl7azy9270dp92srbbr55mgdh9qnk5b1y44lyarmlif1y"))

(define rust-profiling-procmacros-1.0.17
  (crate-source "profiling-procmacros" "1.0.17"
                "0nrxdh5r723raxbs136jmjx46p0c5qgai8jwz4j555mn0ad7ywaj"))

(define rust-proptest-1.10.0
  (crate-source "proptest" "1.10.0"
                "0ch5r381al5z7089j47gkyybzbgygkgld5bzfg019vxcznrnqmip"))

(define rust-proptest-1.11.0
  (crate-source "proptest" "1.11.0"
                "0i27rr5drw4ic8hjzx6i1c6q8s7kmsgpfmzy4m80ys2c6k1gqiab"))

(define rust-puffin-0.16.0
  (crate-source "puffin" "0.16.0"
                "08ass1hfdcq86y7dywa1jylzq57la95rgpcmd6yx82hs9symlhkn"))

(define rust-puffin-0.19.1
  (crate-source "puffin" "0.19.1"
                "07vlkf4i88475a80fhckayzxr9v4pkc21kwvpjkc2bn00mxsx7gs"))

(define rust-puffin-http-0.13.0
  (crate-source "puffin_http" "0.13.0"
                "14w1ihjlv48mpbh114yvgixdqdnzzipnmsg158l3v49m1ihgrgqk"))

(define rust-pxfm-0.1.27
  (crate-source "pxfm" "0.1.27"
                "1a76ydn3wpl2dvyzplv3c6fkx4mkjc9ns60xas9l7alk4n1d71ki"))

(define rust-qoi-0.4.1
  (crate-source "qoi" "0.4.1"
                "00c0wkb112annn2wl72ixyd78mf56p4lxkhlmsggx65l3v3n8vbz"))

(define rust-quick-error-1.2.3
  (crate-source "quick-error" "1.2.3"
                "1q6za3v78hsspisc197bg3g7rpc989qycy8ypr8ap8igv10ikl51"))

(define rust-quick-error-2.0.1
  (crate-source "quick-error" "2.0.1"
                "18z6r2rcjvvf8cn92xjhm2qc3jpd1ljvcbf12zv0k9p565gmb4x9"))

(define rust-quick-xml-0.39.2
  (crate-source "quick-xml" "0.39.2"
                "0z86jkw618p0d7q3zqp7pzh7cnf7wwlanzx8gyma3dffwzl233wm"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-quote-1.0.47
  (crate-source "quote" "1.0.47"
                "00ch0yyzvv6s671ik0kcsbw8nigdaj2g3fr61kcahwx48aqlvgqz"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-rand-0.10.3
  (crate-source "rand" "0.10.3"
                "1bxlhj4m9zrgfgk1yirf0nny86izrngscydfx9w387n9rfbgpjb5"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-0.9.5
  (crate-source "rand" "0.9.5"
                "0hbvllk8g28mqjld6hqmckk69w296qpzg95whm3didsyg46ivvxr"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.10.1
  (crate-source "rand_core" "0.10.1"
                "0s9wiacxrr100icl7i41308gcj85nlcclrc5jx1jd6p10dhigf33"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-rand-xorshift-0.4.0
  (crate-source "rand_xorshift" "0.4.0"
                "0njsn25pis742gb6b89cpq7jp48v9n23a9fvks10yczwks8n4fai"))

(define rust-rav1e-0.8.1
  (crate-source "rav1e" "0.8.1"
                "0axk3ji3jmlr81svmsy5zvj8shmhpp8lz5nyghkq752xx1bdvdj3"))

(define rust-ravif-0.12.0
  (crate-source "ravif" "0.12.0"
                "11dj99rsrdjp12yn4xchxsb78prsg5s8x4smd08qmwgf1jcw2sgg"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-rayon-1.11.0
  (crate-source "rayon" "1.11.0"
                "13x5fxb7rn4j2yw0cr26n7782jkc7rjzmdkg42qxk3xz0p8033rn"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-event-0.4.8
  (crate-source "redox_event" "0.4.8"
                "058v00q7mfnmd60wf5xb0gmx2rwhgqbqvsia6ncm8bvd7mc8s0f5"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-redox-syscall-0.9.4
  (crate-source "redox_syscall" "0.9.4"
                "03p257wq9i52i5h5ns9v0xx03fycgcq17bdcwwqzmil7ka9p0ybk"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-regex-syntax-0.8.11
  (crate-source "regex-syntax" "0.8.11"
                "1m25h5q2wp976fb9gc3dsc9l99svcvd5cri8lncb51c46ydgzxnn"))

(define rust-regex-syntax-0.8.9
  (crate-source "regex-syntax" "0.8.9"
                "0k0a47r1rcl794wj8a948niakbg081s5pp5nlgcbmmr2iy3qfs59"))

(define rust-reis-0.7.1
  (crate-source "reis" "0.7.1"
                "0pzqwqzbjpyjmzhwnwv8g7m7hqdrgfc12kvj41x0chls9337x5jb"))

(define rust-renderdoc-0.11.0
  (crate-source "renderdoc" "0.11.0"
                "04hycbzwqmzw25qnk0lwps70jgxi43cgmkjdvwbyzc183vnajb97"))

(define rust-renderdoc-sys-1.1.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "renderdoc-sys" "1.1.0"
                "0cj8zjs7k0gvchcx3jhpg8r9bbqy8b1hsgbz0flcq2ydn12hmcqr"))

(define rust-rgb-0.8.52
  (crate-source "rgb" "0.8.52"
                "1km115a9lblf9pldvx51dmmg30y8ms4ka67hvas2ndcq556qhshc"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-hash-2.1.3
  (crate-source "rustc-hash" "2.1.3"
                "0bbla578m87qmf3yr55q49l97gxn7z0ha1dwqlnvwwc58ad7y7kb"))

(define rust-rustc-version-0.2.3
  (crate-source "rustc_version" "0.2.3"
                "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.3
  (crate-source "rustix" "1.1.3"
                "0d0z2zcw4rwzni1hm8snw8xdxwwrij336m31c4ghq66cghj9wv0l"))

(define rust-rustix-1.1.4
  (crate-source "rustix" "1.1.4"
                "14511f9yjqh0ix07xjrjpllah3325774gfwi9zpq72sip5jlbzmn"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-rusty-fork-0.3.1
  (crate-source "rusty-fork" "0.3.1"
                "1qkf9rvz2irb1wlbkrhrns8n9hnax48z1lgql5nqyr2fyagzfsyc"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sd-notify-0.4.5
  (crate-source "sd-notify" "0.4.5"
                "1x1bmz30x2i35j771rqyyan40473aqk0xjrh2dk9xdnqf7gylhxr"))

(define rust-semver-0.9.0
  (crate-source "semver" "0.9.0"
                "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-semver-parser-0.7.0
  (crate-source "semver-parser" "0.7.0"
                "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-1.0.229
  (crate-source "serde" "1.0.229"
                "1fp04fq4a79bpm61xz1zy0pbz4kpc7d771zii1k3inmszq55jj21"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-core-1.0.229
  (crate-source "serde_core" "1.0.229"
                "0j1ajiha76h3nmd976il9li6975k121xa7jb39ws8n0yqp4s5p37"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-derive-1.0.229
  (crate-source "serde_derive" "1.0.229"
                "0j4k63i7h1bikxwz2c89ig0hrwbnl9mz1czn85xx99x5cc9dg9g7"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-1.0.4
  (crate-source "serde_spanned" "1.0.4"
                "0xkp0qdzams5sqwndbw3xrhf4c0bb5r46w2ywkp1aqsdb8ggkfzq"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-simd-helpers-0.1.0
  (crate-source "simd_helpers" "0.1.0"
                "19idqicn9k4vhd04ifh2ff41wvna79zphdf2c81rlmpc7f3hz2cm"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-siphasher-1.0.4
  (crate-source "siphasher" "1.0.4"
                "0mn28y43123jdpskdn6r9wibmn066f7h30zkkqn88bd6hj8zxx1k"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-slotmap-1.1.1
  (crate-source "slotmap" "1.1.1"
                "0f20xf53zaysx9ydzkwwqm6hsjyb8lj2j6amhg57iln3jcy8rmdx"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define rust-smithay-client-toolkit-0.21.1
  (crate-source "smithay-client-toolkit" "0.21.1"
                "003zc6547msmwh729lkpm6acjw60rax42qamz4svbp0g9ghrxp3l"))



(define rust-smol-str-0.3.2
  (crate-source "smol_str" "0.3.2"
                "039mj6lc1vkljj17ndlzzkak8kvlmw8ppi6yjdxsh433snfbhxln"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-syn-2.0.119
  (crate-source "syn" "2.0.119"
                "15vjy620l91a3q4n4f4gzhnflmdr6pnm38v2m6cpk86i8av32a47"))

(define rust-syn-3.0.5
  (crate-source "syn" "3.0.5"
                "1yc9vbm1wqs68fyj5f1f3f9isyh6i7wpxccvfrgpfnzn200jxpqj"))

(define rust-syn-3.0.6
  (crate-source "syn" "3.0.6"
                "1vmw7s58rzrs926nv5m06x7qbgswm1aa9iw3s1bj5var47kyi4w5"))

(define rust-synstructure-0.14.0
  (crate-source "synstructure" "0.14.0"
                "00kzyz38xqvs7x1dn6rp64y0ckly4m7ywf3qk3qkgqfzs3nh85wh"))

(define rust-system-deps-7.0.7
  (crate-source "system-deps" "7.0.7"
                "0zsyh2m893nqkp1wri5c85favp2xyl1qpjxnd5nz31pr6qvz7j28"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-tempfile-3.24.0
  (crate-source "tempfile" "3.24.0"
                "171fz3h6rj676miq15fyv1hnv69p426mlp8489bwa1b3xg3sjpb5"))

(define rust-tempfile-3.26.0
  (crate-source "tempfile" "3.26.0"
                "182lfcv9d5w9349i0rjlgn4431k2m3yqfn9ls84p9d3ifxv2r9w2"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-tiff-0.10.3
  (crate-source "tiff" "0.10.3"
                "0vrkdk9cdk07rh7iifcxpn6m8zv3wz695mizhr8rb3gfgzg0b5mg"))

(define rust-tinystr-0.8.4
  (crate-source "tinystr" "0.8.4"
                "0hzncw8rgk4syla79qscfml46jm7ll1zdp7kdacc42cj8n8prqmi"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-toml-0.9.11+spec-1.1.0
  (crate-source "toml" "0.9.11+spec-1.1.0"
                "0ikwmd5s9ndg6afxijaxjcgxw53sd9af3mmfzymf37rh92lckbzk"))

(define rust-toml-0.9.12+spec-1.1.0
  (crate-source "toml" "0.9.12+spec-1.1.0"
                "0qwqbrymqn88mg2yqyq3rj52z6p20448z0jxdbpjsbpwg5g894ng"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-edit-0.23.10+spec-1.0.0
  (crate-source "toml_edit" "0.23.10+spec-1.0.0"
                "0saj5c676j8a3sqaj9akkp09wambg8aflji4zblwwa70azvvkj44"))

(define rust-toml-parser-1.0.6+spec-1.1.0
  (crate-source "toml_parser" "1.0.6+spec-1.1.0"
                "0i5zxv5y3z9g6r3gm6ly4q0hhkahh013q4rys2fz04cf195qn6d3"))

(define rust-toml-parser-1.0.9+spec-1.1.0
  (crate-source "toml_parser" "1.0.9+spec-1.1.0"
                "1i54qpvvcppy8ybdn9gssas81vfzq0kmgkcnxzhyf8w9w0al8bbh"))

(define rust-toml-writer-1.0.6+spec-1.1.0
  (crate-source "toml_writer" "1.0.6+spec-1.1.0"
                "01r6x42d1p8p5kzfsi1fm4dakm3w53vi69f2ivyqpvi1xm5g25mb"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-journald-0.3.2
  (crate-source "tracing-journald" "0.3.2"
                "1l1q4jpwq4jsls1pcjd0wr7djmknwx9w2aqy5dcn5ysv4knq2fid"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-subscriber-0.3.22
  (crate-source "tracing-subscriber" "0.3.22"
                "07hz575a0p1c2i4xw3gs3hkrykhndnkbfhyqdwjhvayx4ww18c1g"))

(define rust-tracy-client-0.18.4
  (crate-source "tracy-client" "0.18.4"
                "19g6g3s5x891k419ahl6y4xnbz100viyjwn7j2mqcpdcmqxzrxm4"))

(define rust-tracy-client-sys-0.28.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "tracy-client-sys" "0.28.0"
                "1gxc1lb3yvbzb8n5069x1gis6vpfdly7n5bj7n8iq37j919wkxy5"))

(define rust-udev-0.9.3
  (crate-source "udev" "0.9.3"
                "17vy1yc6ipb5m2kc2d4lx2qpj45yr7grsjzm3y2gq0a4xblkfkmg"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.20.0
  (crate-source "uuid" "1.20.0"
                "0vwpi7vnwjsfcx58nfks9sgmsz4wpbsk06qlwhgxf34v265x6j7f"))

(define rust-v-frame-0.3.9
  (crate-source "v_frame" "0.3.9"
                "1qkvb4ks33zck931vzqckjn36hkngj6l2cwmvfsnlpc7r0kpfsv6"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.1
  (crate-source "version-compare" "0.2.1"
                "03nziqxwnxlizl42cwsx33vi5xd2cf2jnszhh9rzay7g6xl8bhh3"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-0.2.113
  (crate-source "wasm-bindgen" "0.2.113"
                "1wpg101a5rqqilv4cz4929kbph9g15y4v2fvkbg7yjsrgy9jlwk0"))

(define rust-wasm-bindgen-futures-0.4.63
  (crate-source "wasm-bindgen-futures" "0.4.63"
                "06j4hyxvlfvas7lhvgai44vh8izd59774wv5m8hla3kp1djz92ca"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-0.2.113
  (crate-source "wasm-bindgen-macro" "0.2.113"
                "0l1rbylzb1cs5i6ihmkgk8zic71pg563yadgqj8nnjq9jmiqrb0g"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-macro-support-0.2.113
  (crate-source "wasm-bindgen-macro-support" "0.2.113"
                "0q4xmjmq1c80drv84hz9i9l7fj3yi0v2d11kh1r21p2rc77angxb"))

(define rust-wasm-bindgen-shared-0.2.108
  (crate-source "wasm-bindgen-shared" "0.2.108"
                "04ix7v99rvj5730553j58pqsrwpf9sqazr60y3cchx5cr60ba08z"))

(define rust-wasm-bindgen-shared-0.2.113
  (crate-source "wasm-bindgen-shared" "0.2.113"
                "1d9vdqrzksbfv30bvwy4kc57l08di24775hxq1yshkc2vcdhj3ny"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-wayland-backend-0.3.15
  (crate-source "wayland-backend" "0.3.15"
                "0pbm8j3vv6baqz312biwqfi4qzadbi6nng9v4p3nx4afnlhdsmr8"))

(define rust-wayland-client-0.31.14
  (crate-source "wayland-client" "0.31.14"
                "0i014rcfjgccknnlyfk94fxn4w32l56cpjdmi4qhqsblpfb7qp34"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.31.14
  (crate-source "wayland-cursor" "0.31.14"
                "0kdk7xwj465idk54jf1f24024gdp63wyagca68a176xyh23x2lja"))

(define rust-wayland-egl-0.32.11
  (crate-source "wayland-egl" "0.32.11"
                "0lalq3dzd1x7j7v2dsf9zmwp1m6sy22gyf1gasvxjnwyqjvvv5wv"))

(define rust-wayland-protocols-0.32.13
  (crate-source "wayland-protocols" "0.32.13"
                "1dn4injzx1lnmacnhl3q60m743lvshxmmy0aabb2xaixvq9wil13"))

(define rust-wayland-protocols-experimental-20251230.0.3
  (crate-source "wayland-protocols-experimental" "20251230.0.3"
                "04yg4ny7pkla5iqdswfxvslzxakrphr916a67l4ksmcf2zkw7ili"))

(define rust-wayland-protocols-misc-0.3.12
  (crate-source "wayland-protocols-misc" "0.3.12"
                "1j19dg8h98s153rj2fvbqkghjicdfgjjkr6nvaw0jgpjkrcng5bf"))

(define rust-wayland-protocols-plasma-0.3.10
  (crate-source "wayland-protocols-plasma" "0.3.10"
                "0srh0xhf4jpz8s1kx531lh0ixzx5v6p3iwwpk9d562ih3536765a"))

(define rust-wayland-protocols-wlr-0.3.12
  (crate-source "wayland-protocols-wlr" "0.3.12"
                "0d424vn2hj27r4gjlshm6hy8fcqysr805jkqdjbwgmrng0pya17b"))

(define rust-wayland-scanner-0.31.10
  (crate-source "wayland-scanner" "0.31.10"
                "0jjbsb04pzz8kqiw0wy2ssqx6dqpy70ixrm3ck1vsvnq1y8llclw"))

(define rust-wayland-server-0.31.13
  (crate-source "wayland-server" "0.31.13"
                "1800f2fg41p28q9ddhdv21b78aqahcm9w2aa9zh854f40kmlc66c"))

(define rust-wayland-sys-0.31.11
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "wayland-sys" "0.31.11"
                "1gp3hlkxx13i55lyyi794vnw9a780z3skx0xhj71zr69xwzv5snq"))

(define rust-web-sys-0.3.90
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.90"
                "15wsyn0bmhgf4nkgl23l9fzcqml029jxdlavcbw304lhrsscwpkh"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-weezl-0.1.12
  (crate-source "weezl" "0.1.12"
                "122a1dhha6cib5az4ihcqlh60ns2bi6rskdv875p94lbvj6wk2m2"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-sys-0.45.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.59.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.42.2
  (crate-source "windows-targets" "0.42.2"
                "0wfhnib2fisxlx8c507dbmh97kgij4r6kcxdi0f9nk6l1k080lcf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-winit-0.31.0-beta.3
  (crate-source "winit" "0.31.0-beta.3"
                "0arfj38p8qj6f3sqi04cj193d3l3am76fdxgx8sin3sg7rm1acgl"))

(define rust-winit-android-0.31.0-beta.3
  (crate-source "winit-android" "0.31.0-beta.3"
                "1x2s442594pxfmsgzl3dfb7y9rr2rhs56chy66wqcjan9aa0vmnm"))

(define rust-winit-appkit-0.31.0-beta.3
  (crate-source "winit-appkit" "0.31.0-beta.3"
                "0hmbcakx5517lzxj41jy77vbqqzvcvzsc2c0ydramiksxb03wd13"))

(define rust-winit-common-0.31.0-beta.3
  (crate-source "winit-common" "0.31.0-beta.3"
                "09a0w3pcvr8ckq0j7y5kzfrvphlijw4ivb8xmjm2ll043kzj4179"))

(define rust-winit-core-0.31.0-beta.3
  (crate-source "winit-core" "0.31.0-beta.3"
                "1yfhsd5dy9jh5w9yjv5r1cjilj70h37ia34mk5qq8lbc4c3n7bxq"))

(define rust-winit-orbital-0.31.0-beta.3
  (crate-source "winit-orbital" "0.31.0-beta.3"
                "0l3cry4r131m0ncw5v0f2cr3z8qdd1nnkzfjbp4knalh6zl8m2i7"))

(define rust-winit-uikit-0.31.0-beta.3
  (crate-source "winit-uikit" "0.31.0-beta.3"
                "1da08x4fzbrknrk7d1fq2nahrcic18xmxlr3rxk8973kl4z5lcc3"))

(define rust-winit-wayland-0.31.0-beta.3
  (crate-source "winit-wayland" "0.31.0-beta.3"
                "00zqf5dykispphvy711kf0z43qdb0qhi01ggds44a36y0vnwwr4b"))

(define rust-winit-web-0.31.0-beta.3
  (crate-source "winit-web" "0.31.0-beta.3"
                "1jgic0vcpi9z9lm8acs1xi5dc9fr73prlml0zfyrghjwz52vwfgm"))

(define rust-winit-win32-0.31.0-beta.3
  (crate-source "winit-win32" "0.31.0-beta.3"
                "1v5nl3kng4cznp52snxv4afqlgq1fjnz7jsnyr8x60fz8464iyr5"))

(define rust-winit-x11-0.31.0-beta.3
  (crate-source "winit-x11" "0.31.0-beta.3"
                "1alnzmzrhvxffk6jhsg1z5vh1dqcgmpiq6j9jmw4nlbyj36lcmxv"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

(define rust-winnow-1.0.4
  (crate-source "winnow" "1.0.4"
                "10fzxipa7lx16172p3aca9j60hzbqgjki2f95kqksd5qywcp7f93"))

(define rust-wio-0.2.2
  (crate-source "wio" "0.2.2"
                "199p404fp96w1f1c93bf1jrvaqwypxf3hmmldhww4jk4yhr9j4jx"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-wlcs-0.1.0
  (crate-source "wlcs" "0.1.0"
                "17k0nwn3f2z71rncb8glb4x15m5zmcbklnk71hpv739nrq2w769d"))

(define rust-writeable-0.6.4
  (crate-source "writeable" "0.6.4"
                "1p3r4s4wbf3dksfpj3xyrn7id5p0f7r74mj6qx6ngjfd6cm2vn1s"))

(define rust-x11-dl-2.21.0
  (crate-source "x11-dl" "2.21.0"
                "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))

(define rust-x11rb-0.13.2
  (crate-source "x11rb" "0.13.2"
                "053lvnaw9ycbl791mgwly2hw27q6vqgzrb1y5kz1as52wmdsm4wr"))

(define rust-x11rb-protocol-0.13.2
  (crate-source "x11rb-protocol" "0.13.2"
                "1g81cznbyn522b0fbis0i44wh3adad2vhsz5pzf99waf3sbc4vza"))

(define rust-xcursor-0.3.10
  (crate-source "xcursor" "0.3.10"
                "0awgy98awg4ydcfmynqfcwvl4bnnfcm4i2vvnk2n926a02jy9jdy"))

(define rust-xkbcommon-0.7.0
  (crate-source "xkbcommon" "0.7.0"
                "07n9shhcls66wjvmk5pzqql46ipfdv7b8hbc384wgv9hk4jpv1hk"))

(define rust-xkbcommon-0.8.0
  (crate-source "xkbcommon" "0.8.0"
                "1j8s1sfwc6bw9phsca65rw3q3b5l2651v1s0pk5yxm6baa9wlrld"))

(define rust-xkbcommon-0.9.0
  (crate-source "xkbcommon" "0.9.0"
                "0bd0qkapxsvblfw42x6ryhi50d63v55g40awf2alx8b0h3s79ad7"))

(define rust-xkbcommon-dl-0.4.2
  (crate-source "xkbcommon-dl" "0.4.2"
                "1iai0r3b5skd9vbr8z5b0qixiz8jblzfm778ddm8ba596a0dwffh"))

(define rust-xkeysym-0.2.1
  (crate-source "xkeysym" "0.2.1"
                "0mksx670cszyd7jln6s7dhkw11hdfv7blwwr3isq98k22ljh1k5r"))

(define rust-xml-rs-0.8.28
  (crate-source "xml-rs" "0.8.28"
                "0grdj7xwbki5zrkalrg8dljyf14y4yj3wrj34sbzqp06i9zk7s1s"))

(define rust-y4m-0.8.0
  (crate-source "y4m" "0.8.0"
                "0j24y2zf60lpxwd7kyg737hqfyqx16y32s0fjyi6fax6w4hlnnks"))

(define rust-yoke-0.8.3
  (crate-source "yoke" "0.8.3"
                "1xgyj6c2lxj2bp891ynmhws87c6z7yyv2li1v0ss9di40hxf57vh"))

(define rust-yoke-derive-0.8.3
  (crate-source "yoke-derive" "0.8.3"
                "0y1a857vmqk2zpq4jj4sxxm7mla18xsrapjldpmvq3g4pql1909k"))

(define rust-zbus-5.19.0
  (crate-source "zbus" "5.19.0"
                "01sram5sgwsg3x8mghx77cjbsfa2c10mar7fnzj23d2w0xybxd2x"))

(define rust-zbus-macros-5.19.0
  (crate-source "zbus_macros" "5.19.0"
                "0h4gr26kyhdyn503rgg8h44sjxm8d6n8qbzpd0cdzrmd15fn7419"))

(define rust-zbus-names-4.3.4
  (crate-source "zbus_names" "4.3.4"
                "0kk250s3x1fxpz9fvhdr64ydbacpn8ah23hy021yhlzzlfs8igyq"))

(define rust-zcheapstr-1.1.0
  (crate-source "zcheapstr" "1.1.0"
                "0wwlv70bi2rydvvzfq249q6i51mjx85c4m2wxcx1hra5c18yrbyi"))

(define rust-zerocopy-0.8.38
  (crate-source "zerocopy" "0.8.38"
                "0qd9jka18x3p1ifn8nxsx9gw8ldavybzqbc5x4g728svhnk3mksp"))

(define rust-zerocopy-0.8.39
  (crate-source "zerocopy" "0.8.39"
                "0jmf1iqns5sq07k3dscsgyc706pycar67rrq4j9nrnzacgb3avfv"))

(define rust-zerocopy-0.8.57
  (crate-source "zerocopy" "0.8.57"
                "0wr85plmrkz0nh5grbqap23l9ghqq4mq1ip4x759q23dyflh4lfk"))

(define rust-zerocopy-derive-0.8.38
  (crate-source "zerocopy-derive" "0.8.38"
                "0xfbq46cryk6w4lcwikyg27r8mzgfynrcrgyqhvpha0smy86jqca"))

(define rust-zerocopy-derive-0.8.39
  (crate-source "zerocopy-derive" "0.8.39"
                "05z5yfq0mx3xdqadrgq5sd4d03nl82d9r0vp1qchaip9d4qws8j1"))

(define rust-zerocopy-derive-0.8.57
  (crate-source "zerocopy-derive" "0.8.57"
                "1g5js10p1q1d7wn6b74wjvrjznv34x5cfxpj7jj8s9a4mgsh2v0l"))

(define rust-zerofrom-0.1.8
  (crate-source "zerofrom" "0.1.8"
                "0wjjdj7gdmd0iq91gzkxl7dlv0nhkk80l4bmdpzh3a1yh48mmh0f"))

(define rust-zerofrom-derive-0.1.8
  (crate-source "zerofrom-derive" "0.1.8"
                "0jhdy4p9c6ja3dynsdck14g2iilq8ai5ch6nsi455x67ys1lcnzp"))

(define rust-zerotrie-0.2.5
  (crate-source "zerotrie" "0.2.5"
                "0gss16krjzk22m57dz5hkdjg99ibj6pa41qr68na7w1jpp1nk8jf"))

(define rust-zerovec-0.11.8
  (crate-source "zerovec" "0.11.8"
                "1n3xlvyba8riys9s8awy4xp533phqycr78nbsmvdkh86g3hn815v"))

(define rust-zerovec-derive-0.11.6
  (crate-source "zerovec-derive" "0.11.6"
                "1ni5j8v99x3fcf3l8kp64b7aq4vf8y22jshfq74xs9mxkp1nzprl"))

(define rust-zmij-1.0.19
  (crate-source "zmij" "1.0.19"
                "0i9lpsfa4sgq52dnrli9z3sc2rllwawyc6jp6x38jf4hma65zw1z"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-zune-core-0.4.12
  (crate-source "zune-core" "0.4.12"
                "0jj1ra86klzlcj9aha9als9d1dzs7pqv3azs1j3n96822wn3lhiz"))

(define rust-zune-core-0.5.1
  (crate-source "zune-core" "0.5.1"
                "1ya0zdqxlr5v57791j7bvm408ri2cfx81a4v6z85f560yw3hi2nb"))

(define rust-zune-inflate-0.2.54
  (crate-source "zune-inflate" "0.2.54"
                "00kg24jh3zqa3i6rg6yksnb71bch9yi1casqydl00s7nw8pk7avk"))

(define rust-zune-jpeg-0.4.21
  (crate-source "zune-jpeg" "0.4.21"
                "04r7g6y9jp7d4c9bq23rz3gwzlr1dsl7vdk4yly35bc4jf52rki9"))

(define rust-zune-jpeg-0.5.12
  (crate-source "zune-jpeg" "0.5.12"
                "1zipxj775zwgnvarcx66w7x688f3v6wgsb0whgihkirlyv79w3j1"))

(define rust-zvariant-5.15.0
  (crate-source "zvariant" "5.15.0"
                "0iwihslxshfhalihp6kv7xz7nbv1p3b9sl97hi2izpbcrhklrly1"))

(define rust-zvariant-derive-5.15.0
  (crate-source "zvariant_derive" "5.15.0"
                "15y4z1rkcpvrz7dv7j2rfv8wiq6i8nzifj9pgw6dnlj3kgk5ahc6"))

(define rust-zvariant-utils-4.2.0
  (crate-source "zvariant_utils" "4.2.0"
                "18q80094ci64myzvcp0g2l3c6mnx7b3hsii8lfabc853c51jkl5s"))

(define-cargo-inputs lookup-cargo-inputs
  (emacs-ewm =>
             (list rust-aho-corasick-1.1.4
                   rust-aliasable-0.1.3
                   rust-annotate-snippets-0.11.5
                   rust-anstyle-1.0.13
                   rust-anyhow-1.0.101
                   rust-appendlist-1.4.0
                   rust-async-broadcast-0.7.2
                   rust-async-channel-2.5.0
                   rust-async-executor-1.13.3
                   rust-async-io-2.6.0
                   rust-async-lock-3.4.2
                   rust-async-process-2.5.0
                   rust-async-recursion-1.1.1
                   rust-async-signal-0.2.13
                   rust-async-task-4.7.1
                   rust-async-trait-0.1.89
                   rust-atomic-waker-1.1.2
                   rust-autocfg-1.5.0
                   rust-bindgen-0.72.1
                   rust-bit-set-0.8.0
                   rust-bit-vec-0.8.0
                   rust-bitflags-2.10.0
                   rust-blocking-1.6.2
                   rust-bumpalo-3.19.1
                   rust-bytemuck-1.25.0
                   rust-bytemuck-derive-1.10.2
                   rust-calloop-0.14.3
                   rust-cc-1.2.55
                   rust-cexpr-0.6.0
                   rust-cfg-expr-0.20.6
                   rust-cfg-if-1.0.4
                   rust-chacha20-0.10.2
                   rust-clang-sys-1.8.1
                   rust-concurrent-queue-2.5.0
                   rust-console-0.15.11
                   rust-cookie-factory-0.3.3
                   rust-cpufeatures-0.3.1
                   rust-crossbeam-utils-0.8.21
                   rust-ctor-0.1.26
                   rust-cursor-icon-1.2.0
                   rust-darling-0.10.2
                   rust-darling-core-0.10.2
                   rust-darling-macro-0.10.2
                   rust-dlib-0.5.3
                   rust-downcast-rs-1.2.1
                   rust-drm-0.14.1
                   rust-drm-ffi-0.9.1
                   rust-drm-fourcc-2.2.0
                   rust-drm-sys-0.8.1
                   rust-either-1.15.0
                   rust-elisp
                   rust-emacs-0.21.0
                   rust-emacs-macros-0.17.0
                   rust-emacs-module-0.21.0
                   rust-encode-unicode-1.0.0
                   rust-endi-1.1.1
                   rust-enumflags2-0.7.12
                   rust-enumflags2-derive-0.7.12
                   rust-equivalent-1.0.2
                   rust-errno-0.3.14
                   rust-event-listener-5.4.1
                   rust-event-listener-strategy-0.5.4
                   rust-fastrand-2.3.0
                   rust-find-msvc-tools-0.1.9
                   rust-fnv-1.0.7
                   rust-futures-channel-0.3.31
                   rust-futures-core-0.3.31
                   rust-futures-executor-0.3.31
                   rust-futures-io-0.3.31
                   rust-futures-lite-2.6.1
                   rust-futures-macro-0.3.31
                   rust-futures-task-0.3.32
                   rust-futures-util-0.3.31
                   rust-gbm-0.18.0
                   rust-gbm-sys-0.4.0
                   rust-generator-0.8.8
                   rust-getrandom-0.3.4
                   rust-getrandom-0.4.3
                   rust-gio-0.20.12
                   rust-gio-sys-0.20.10
                   rust-gl-generator-0.14.0
                   rust-glam-0.33.11
                   rust-glib-0.20.12
                   rust-glib-macros-0.20.12
                   rust-glib-sys-0.20.10
                   rust-glob-0.3.3
                   rust-gobject-sys-0.20.10
                   rust-hashbrown-0.16.1
                   rust-heck-0.5.0
                   rust-hermit-abi-0.3.9
                   rust-hermit-abi-0.5.2
                   rust-hex-0.4.3
                   rust-ident-case-1.0.1
                   rust-indexmap-2.13.0
                   rust-input-0.10.0
                   rust-input-sys-1.19.0
                   rust-insta-1.46.3
                   rust-io-lifetimes-1.0.11
                   rust-itertools-0.12.1
                   rust-itoa-1.0.17
                   rust-js-sys-0.3.85
                   rust-khronos-api-3.1.0
                   rust-lazy-static-1.5.0
                   rust-libc-0.2.180
                   rust-libdisplay-info-0.3.0
                   rust-libdisplay-info-derive-0.1.1
                   rust-libdisplay-info-sys-0.3.0
                   rust-libloading-0.8.9
                   rust-libseat-0.2.4
                   rust-libseat-sys-0.2.0
                   rust-libspa-0.10.1
                   rust-libspa-sys-0.10.1
                   rust-libudev-sys-0.1.4
                   rust-linux-raw-sys-0.4.15
                   rust-linux-raw-sys-0.9.4
                   rust-linux-raw-sys-0.11.0
                   rust-log-0.4.29
                   rust-loom-0.7.2
                   rust-matchers-0.1.0
                   rust-memchr-2.7.6
                   rust-memmap2-0.9.9
                   rust-memoffset-0.9.1
                   rust-minimal-lexical-0.2.1
                   rust-nom-7.1.3
                   rust-nom-8.0.0
                   rust-nu-ansi-term-0.46.0
                   rust-num-traits-0.2.19
                   rust-once-cell-1.21.3
                   rust-ordered-stream-0.2.0
                   rust-overload-0.1.1
                   rust-parking-2.2.1
                   rust-pin-project-lite-0.2.16
                   rust-pin-utils-0.1.0
                   rust-piper-0.2.4
                   rust-pipewire-0.10.1
                   rust-pipewire-sys-0.10.1
                   rust-pkg-config-0.3.32
                   rust-polling-3.11.0
                   rust-portable-atomic-1.15.0
                   rust-ppv-lite86-0.2.21
                   rust-proc-macro-crate-3.4.0
                   rust-proc-macro2-1.0.106
                   rust-profiling-1.0.17
                   rust-profiling-procmacros-1.0.17
                   rust-proptest-1.10.0
                   rust-quick-error-1.2.3
                   rust-quick-xml-0.39.2
                   rust-quote-1.0.44
                   rust-r-efi-5.3.0
                   rust-r-efi-6.0.0
                   rust-rand-0.9.2
                   rust-rand-0.10.3
                   rust-rand-chacha-0.9.0
                   rust-rand-core-0.9.5
                   rust-rand-core-0.10.1
                   rust-rand-xorshift-0.4.0
                   rust-regex-1.12.3
                   rust-regex-automata-0.1.10
                   rust-regex-automata-0.4.14
                   rust-regex-syntax-0.6.29
                   rust-regex-syntax-0.8.9
                   rust-reis-0.7.1
                   rust-rustc-hash-2.1.1
                   rust-rustc-version-0.2.3
                   rust-rustix-0.38.44
                   rust-rustix-1.1.3
                   rust-rustversion-1.0.22
                   rust-rusty-fork-0.3.1
                   rust-scoped-tls-1.0.1
                   rust-sd-notify-0.4.5
                   rust-semver-0.9.0
                   rust-semver-1.0.27
                   rust-semver-parser-0.7.0
                   rust-serde-1.0.228
                   rust-serde-core-1.0.228
                   rust-serde-derive-1.0.228
                   rust-serde-json-1.0.149
                   rust-serde-repr-0.1.20
                   rust-serde-spanned-1.0.4
                   rust-sharded-slab-0.1.7
                   rust-shlex-1.3.0
                   rust-signal-hook-registry-1.4.8
                   rust-similar-2.7.0
                   rust-siphasher-1.0.4
                   rust-slab-0.4.12
                   rust-smallvec-1.15.1
                   rust-smithay-0.7.0.79bbed5
                   rust-strsim-0.9.3
                   rust-syn-1.0.109
                   rust-syn-2.0.114
                   rust-syn-3.0.6
                   rust-system-deps-7.0.7
                   rust-target-lexicon-0.13.3
                   rust-tempfile-3.24.0
                   rust-thiserror-1.0.69
                   rust-thiserror-2.0.18
                   rust-thiserror-impl-1.0.69
                   rust-thiserror-impl-2.0.18
                   rust-thread-local-1.1.9
                   rust-toml-0.9.11+spec-1.1.0
                   rust-toml-datetime-0.7.5+spec-1.1.0
                   rust-toml-edit-0.23.10+spec-1.0.0
                   rust-toml-parser-1.0.6+spec-1.1.0
                   rust-toml-writer-1.0.6+spec-1.1.0
                   rust-tracing-0.1.44
                   rust-tracing-attributes-0.1.31
                   rust-tracing-core-0.1.36
                   rust-tracing-journald-0.3.2
                   rust-tracing-log-0.2.0
                   rust-tracing-subscriber-0.3.19
                   rust-tracy-client-0.18.4
                   rust-tracy-client-sys-0.28.0
                   rust-udev-0.9.3
                   rust-uds-windows-1.1.0
                   rust-unarray-0.1.4
                   rust-unicode-ident-1.0.22
                   rust-unicode-width-0.2.2
                   rust-uuid-1.20.0
                   rust-valuable-0.1.1
                   rust-version-compare-0.2.1
                   rust-wait-timeout-0.2.1
                   rust-wasip2-1.0.2+wasi-0.2.9
                   rust-wasm-bindgen-0.2.108
                   rust-wasm-bindgen-macro-0.2.108
                   rust-wasm-bindgen-macro-support-0.2.108
                   rust-wasm-bindgen-shared-0.2.108
                   rust-wayland-backend-0.3.15
                   rust-wayland-client-0.31.14
                   rust-wayland-protocols-0.32.13
                   rust-wayland-protocols-misc-0.3.12
                   rust-wayland-protocols-wlr-0.3.12
                   rust-wayland-scanner-0.31.10
                   rust-wayland-server-0.31.13
                   rust-wayland-sys-0.31.11
                   rust-winapi-0.3.9
                   rust-winapi-i686-pc-windows-gnu-0.4.0
                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                   rust-windows-link-0.2.1
                   rust-windows-result-0.4.1
                   rust-windows-sys-0.48.0
                   rust-windows-sys-0.59.0
                   rust-windows-sys-0.61.2
                   rust-windows-targets-0.48.5
                   rust-windows-targets-0.52.6
                   rust-windows-aarch64-gnullvm-0.48.5
                   rust-windows-aarch64-gnullvm-0.52.6
                   rust-windows-aarch64-msvc-0.48.5
                   rust-windows-aarch64-msvc-0.52.6
                   rust-windows-i686-gnu-0.48.5
                   rust-windows-i686-gnu-0.52.6
                   rust-windows-i686-gnullvm-0.52.6
                   rust-windows-i686-msvc-0.48.5
                   rust-windows-i686-msvc-0.52.6
                   rust-windows-x86-64-gnu-0.48.5
                   rust-windows-x86-64-gnu-0.52.6
                   rust-windows-x86-64-gnullvm-0.48.5
                   rust-windows-x86-64-gnullvm-0.52.6
                   rust-windows-x86-64-msvc-0.48.5
                   rust-windows-x86-64-msvc-0.52.6
                   rust-winnow-0.7.14
                   rust-winnow-1.0.4
                   rust-wit-bindgen-0.51.0
                   rust-xcursor-0.3.10
                   rust-xkbcommon-0.9.0
                   rust-xkeysym-0.2.1
                   rust-xml-rs-0.8.28
                   rust-zbus-5.19.0
                   rust-zbus-macros-5.19.0
                   rust-zbus-names-4.3.4
                   rust-zcheapstr-1.1.0
                   rust-zerocopy-0.8.38
                   rust-zerocopy-derive-0.8.38
                   rust-zmij-1.0.19
                   rust-zvariant-5.15.0
                   rust-zvariant-derive-5.15.0
                   rust-zvariant-utils-4.2.0))
  (rust-elisp =>
              (list rust-anyhow-1.0.104
                    rust-autocfg-1.5.1
                    rust-bit-set-0.8.0
                    rust-bit-vec-0.8.0
                    rust-bitflags-2.13.2
                    rust-cfg-if-1.0.4
                    rust-ctor-0.1.26
                    rust-darling-0.10.2
                    rust-darling-core-0.10.2
                    rust-darling-macro-0.10.2
                    rust-emacs-0.21.0
                    rust-emacs-macros-0.17.0
                    rust-emacs-module-0.21.0
                    rust-errno-0.3.14
                    rust-fastrand-2.5.0
                    rust-fnv-1.0.7
                    rust-getrandom-0.3.4
                    rust-getrandom-0.4.3
                    rust-ident-case-1.0.1
                    rust-libc-0.2.189
                    rust-linux-raw-sys-0.12.1
                    rust-num-traits-0.2.19
                    rust-once-cell-1.21.4
                    rust-ppv-lite86-0.2.21
                    rust-proc-macro2-1.0.107
                    rust-proptest-1.11.0
                    rust-quick-error-1.2.3
                    rust-quote-1.0.47
                    rust-r-efi-5.3.0
                    rust-r-efi-6.0.0
                    rust-rand-0.9.5
                    rust-rand-chacha-0.9.0
                    rust-rand-core-0.9.5
                    rust-rand-xorshift-0.4.0
                    rust-regex-syntax-0.8.11
                    rust-rustc-version-0.2.3
                    rust-rustix-1.1.4
                    rust-rusty-fork-0.3.1
                    rust-semver-0.9.0
                    rust-semver-parser-0.7.0
                    rust-serde-1.0.229
                    rust-serde-core-1.0.229
                    rust-serde-derive-1.0.229
                    rust-strsim-0.9.3
                    rust-syn-1.0.109
                    rust-syn-2.0.119
                    rust-syn-3.0.5
                    rust-tempfile-3.27.0
                    rust-thiserror-1.0.69
                    rust-thiserror-impl-1.0.69
                    rust-unarray-0.1.4
                    rust-unicode-ident-1.0.24
                    rust-wait-timeout-0.2.1
                    rust-wasip2-1.0.2+wasi-0.2.9
                    rust-windows-link-0.2.1
                    rust-windows-sys-0.61.2
                    rust-wit-bindgen-0.51.0
                    rust-zerocopy-0.8.57
                    rust-zerocopy-derive-0.8.57))
  (rust-smithay =>
                (list rust-adler2-2.0.1
                      rust-aho-corasick-1.1.4
                      rust-aliasable-0.1.3
                      rust-aligned-0.4.3
                      rust-aligned-vec-0.6.4
                      rust-android-activity-0.6.0
                      rust-android-properties-0.2.2
                      rust-anes-0.1.6
                      rust-anstream-0.6.21
                      rust-anstyle-1.0.13
                      rust-anstyle-parse-0.2.7
                      rust-anstyle-query-1.1.5
                      rust-anstyle-wincon-3.0.11
                      rust-anyhow-1.0.102
                      rust-appendlist-1.4.0
                      rust-arbitrary-1.4.2
                      rust-arg-enum-proc-macro-0.3.4
                      rust-arrayvec-0.7.6
                      rust-as-raw-xcb-connection-1.0.1
                      rust-as-slice-0.2.1
                      rust-ash-0.38.0+1.3.281
                      rust-atomic-waker-1.1.2
                      rust-autocfg-1.5.0
                      rust-av-scenechange-0.14.1
                      rust-av1-grain-0.2.5
                      rust-avif-serialize-0.8.8
                      rust-bincode-1.3.3
                      rust-bindgen-0.69.5
                      rust-bindgen-0.70.1
                      rust-bindgen-0.72.1
                      rust-bit-field-0.10.3
                      rust-bitflags-1.3.2
                      rust-bitflags-2.11.0
                      rust-bitstream-io-4.9.0
                      rust-block2-0.6.2
                      rust-borsh-1.8.1
                      rust-built-0.8.0
                      rust-bumpalo-3.20.2
                      rust-bytemuck-1.25.0
                      rust-bytemuck-derive-1.10.2
                      rust-byteorder-1.5.0
                      rust-byteorder-lite-0.1.0
                      rust-bytes-1.11.1
                      rust-calloop-0.13.0
                      rust-calloop-0.14.4
                      rust-calloop-wayland-source-0.3.0
                      rust-calloop-wayland-source-0.4.1
                      rust-cast-0.3.0
                      rust-cc-1.2.56
                      rust-cesu8-1.1.0
                      rust-cexpr-0.6.0
                      rust-cfg-expr-0.20.6
                      rust-cfg-if-1.0.4
                      rust-cfg-aliases-0.2.2
                      rust-chacha20-0.10.2
                      rust-ciborium-0.2.2
                      rust-ciborium-io-0.2.2
                      rust-ciborium-ll-0.2.2
                      rust-clang-sys-1.8.1
                      rust-clap-4.5.60
                      rust-clap-builder-4.5.60
                      rust-clap-derive-4.5.55
                      rust-clap-lex-1.0.0
                      rust-color-quant-1.1.0
                      rust-colorchoice-1.0.4
                      rust-combine-4.6.7
                      rust-concurrent-queue-2.5.0
                      rust-container-of-0.5.1
                      rust-core2-0.4.0
                      rust-cpufeatures-0.3.1
                      rust-crc32fast-1.5.0
                      rust-criterion-0.5.1
                      rust-criterion-plot-0.5.0
                      rust-crossbeam-channel-0.5.15
                      rust-crossbeam-deque-0.8.6
                      rust-crossbeam-epoch-0.9.18
                      rust-crossbeam-utils-0.8.21
                      rust-crunchy-0.2.4
                      rust-cursor-icon-1.2.0
                      rust-dispatch2-0.3.1
                      rust-displaydoc-0.2.7
                      rust-dlib-0.5.3
                      rust-downcast-rs-1.2.1
                      rust-dpi-0.1.2
                      rust-drm-0.14.1
                      rust-drm-ffi-0.9.1
                      rust-drm-fourcc-2.2.0
                      rust-drm-sys-0.8.1
                      rust-either-1.15.0
                      rust-encoding-rs-0.8.35
                      rust-enumflags2-0.7.12
                      rust-enumflags2-derive-0.7.12
                      rust-equator-0.4.2
                      rust-equator-macro-0.4.2
                      rust-equivalent-1.0.2
                      rust-errno-0.3.14
                      rust-exr-1.74.0
                      rust-fastrand-2.3.0
                      rust-fax-0.2.6
                      rust-fax-derive-0.2.0
                      rust-fdeflate-0.3.7
                      rust-find-msvc-tools-0.1.9
                      rust-flate2-1.1.9
                      rust-float-cmp-0.9.0
                      rust-foldhash-0.1.5
                      rust-foldhash-0.2.0
                      rust-form-urlencoded-1.2.2
                      rust-fps-ticker-1.0.0
                      rust-futures-core-0.3.32
                      rust-futures-task-0.3.32
                      rust-futures-util-0.3.32
                      rust-gbm-0.18.0
                      rust-gbm-sys-0.4.0
                      rust-generator-0.8.8
                      rust-gethostname-1.1.0
                      rust-getrandom-0.2.17
                      rust-getrandom-0.3.4
                      rust-getrandom-0.4.1
                      rust-gif-0.14.1
                      rust-gl-generator-0.14.0
                      rust-glam-0.33.11
                      rust-glob-0.3.3
                      rust-glow-0.16.0
                      rust-half-2.7.1
                      rust-hashbrown-0.15.5
                      rust-hashbrown-0.16.1
                      rust-heck-0.5.0
                      rust-hermit-abi-0.3.9
                      rust-hermit-abi-0.5.2
                      rust-home-0.5.11
                      rust-icu-collections-2.2.0
                      rust-icu-locale-core-2.2.0
                      rust-icu-normalizer-2.2.0
                      rust-icu-normalizer-data-2.2.0
                      rust-icu-properties-2.2.0
                      rust-icu-properties-data-2.2.0
                      rust-icu-provider-2.2.0
                      rust-id-arena-2.3.0
                      rust-idna-1.1.0
                      rust-idna-adapter-1.2.2
                      rust-image-0.25.9
                      rust-image-webp-0.2.4
                      rust-imgref-1.12.0
                      rust-indexmap-2.13.0
                      rust-input-0.10.0
                      rust-input-sys-1.19.0
                      rust-instant-0.1.13
                      rust-interpolate-name-0.2.4
                      rust-io-lifetimes-1.0.11
                      rust-is-terminal-0.4.17
                      rust-is-terminal-polyfill-1.70.2
                      rust-itertools-0.10.5
                      rust-itertools-0.12.1
                      rust-itertools-0.14.0
                      rust-itoa-1.0.17
                      rust-jni-0.21.1
                      rust-jni-sys-0.3.0
                      rust-jobserver-0.1.34
                      rust-js-sys-0.3.90
                      rust-keyboard-types-0.8.3
                      rust-khronos-api-3.1.0
                      rust-lazy-static-1.5.0
                      rust-lazycell-1.3.0
                      rust-leb128fmt-0.1.0
                      rust-lebe-0.5.3
                      rust-libc-0.2.182
                      rust-libdisplay-info-0.3.0
                      rust-libdisplay-info-derive-0.1.1
                      rust-libdisplay-info-sys-0.3.0
                      rust-libfuzzer-sys-0.4.12
                      rust-libloading-0.7.4
                      rust-libloading-0.8.9
                      rust-libredox-0.1.25
                      rust-libseat-0.2.4
                      rust-libseat-sys-0.2.0
                      rust-libudev-sys-0.1.4
                      rust-linux-raw-sys-0.4.15
                      rust-linux-raw-sys-0.9.4
                      rust-linux-raw-sys-0.12.1
                      rust-litemap-0.8.3
                      rust-lock-api-0.4.14
                      rust-log-0.4.29
                      rust-loom-0.7.2
                      rust-loop9-0.1.5
                      rust-lz4-flex-0.10.0
                      rust-matchers-0.2.0
                      rust-maybe-rayon-0.1.1
                      rust-memchr-2.8.0
                      rust-memmap2-0.8.0
                      rust-memmap2-0.9.10
                      rust-memoffset-0.6.5
                      rust-memoffset-0.9.1
                      rust-minimal-lexical-0.2.1
                      rust-miniz-oxide-0.8.9
                      rust-moxcms-0.7.11
                      rust-ndk-0.9.0
                      rust-ndk-context-0.1.1
                      rust-ndk-sys-0.6.0+11769913
                      rust-new-debug-unreachable-1.0.6
                      rust-nix-0.27.1
                      rust-nom-7.1.3
                      rust-nom-8.0.0
                      rust-noop-proc-macro-0.3.0
                      rust-nu-ansi-term-0.50.3
                      rust-num-bigint-0.4.6
                      rust-num-derive-0.4.2
                      rust-num-integer-0.1.46
                      rust-num-rational-0.4.2
                      rust-num-traits-0.2.19
                      rust-num-enum-0.7.5
                      rust-num-enum-derive-0.7.5
                      rust-objc2-0.6.4
                      rust-objc2-app-kit-0.3.2
                      rust-objc2-core-foundation-0.3.2
                      rust-objc2-core-graphics-0.3.2
                      rust-objc2-core-video-0.3.2
                      rust-objc2-encode-4.1.0
                      rust-objc2-foundation-0.3.2
                      rust-objc2-ui-kit-0.3.2
                      rust-once-cell-1.21.3
                      rust-once-cell-polyfill-1.70.2
                      rust-oorandom-11.1.5
                      rust-orbclient-0.4.6
                      rust-parking-lot-0.12.5
                      rust-parking-lot-core-0.9.12
                      rust-paste-1.0.15
                      rust-pastey-0.1.1
                      rust-percent-encoding-2.3.2
                      rust-pin-project-1.1.10
                      rust-pin-project-internal-1.1.10
                      rust-pin-project-lite-0.2.16
                      rust-pixman-0.2.1
                      rust-pixman-sys-0.1.0
                      rust-pkg-config-0.3.32
                      rust-plain-0.2.3
                      rust-plotters-0.3.7
                      rust-plotters-backend-0.3.7
                      rust-plotters-svg-0.3.7
                      rust-png-0.18.1
                      rust-polling-3.11.0
                      rust-portable-atomic-1.15.0
                      rust-potential-utf-0.1.6
                      rust-ppv-lite86-0.2.21
                      rust-prettyplease-0.2.37
                      rust-proc-macro-crate-3.4.0
                      rust-proc-macro2-1.0.106
                      rust-profiling-1.0.17
                      rust-profiling-procmacros-1.0.17
                      rust-puffin-0.16.0
                      rust-puffin-0.19.1
                      rust-puffin-http-0.13.0
                      rust-pxfm-0.1.27
                      rust-qoi-0.4.1
                      rust-quick-error-2.0.1
                      rust-quick-xml-0.39.2
                      rust-quote-1.0.44
                      rust-r-efi-5.3.0
                      rust-rand-0.8.5
                      rust-rand-0.9.2
                      rust-rand-0.10.3
                      rust-rand-chacha-0.3.1
                      rust-rand-chacha-0.9.0
                      rust-rand-core-0.6.4
                      rust-rand-core-0.9.5
                      rust-rand-core-0.10.1
                      rust-rav1e-0.8.1
                      rust-ravif-0.12.0
                      rust-raw-window-handle-0.6.2
                      rust-rayon-1.11.0
                      rust-rayon-core-1.13.0
                      rust-redox-event-0.4.8
                      rust-redox-syscall-0.5.18
                      rust-redox-syscall-0.9.4
                      rust-regex-1.12.3
                      rust-regex-automata-0.4.14
                      rust-regex-syntax-0.8.10
                      rust-reis-0.7.1
                      rust-renderdoc-0.11.0
                      rust-renderdoc-sys-1.1.0
                      rust-rgb-0.8.52
                      rust-rustc-hash-1.1.0
                      rust-rustc-hash-2.1.3
                      rust-rustix-0.38.44
                      rust-rustix-1.1.4
                      rust-rustversion-1.0.22
                      rust-same-file-1.0.6
                      rust-scoped-tls-1.0.1
                      rust-scopeguard-1.2.0
                      rust-semver-1.0.27
                      rust-serde-1.0.228
                      rust-serde-core-1.0.228
                      rust-serde-derive-1.0.228
                      rust-serde-json-1.0.149
                      rust-serde-spanned-1.0.4
                      rust-sharded-slab-0.1.7
                      rust-shlex-1.3.0
                      rust-simd-adler32-0.3.8
                      rust-simd-helpers-0.1.0
                      rust-siphasher-1.0.4
                      rust-slab-0.4.12
                      rust-slotmap-1.1.1
                      rust-smallvec-1.15.1
                      rust-smithay-client-toolkit-0.19.2
                      rust-smithay-client-toolkit-0.21.1
                      rust-smol-str-0.3.2
                      rust-stable-deref-trait-1.2.1
                      rust-strsim-0.11.1
                      rust-syn-2.0.117
                      rust-syn-3.0.6
                      rust-synstructure-0.14.0
                      rust-system-deps-7.0.7
                      rust-target-lexicon-0.13.3
                      rust-tempfile-3.26.0
                      rust-thiserror-1.0.69
                      rust-thiserror-2.0.18
                      rust-thiserror-impl-1.0.69
                      rust-thiserror-impl-2.0.18
                      rust-thread-local-1.1.9
                      rust-tiff-0.10.3
                      rust-tinystr-0.8.4
                      rust-tinytemplate-1.2.1
                      rust-toml-0.9.12+spec-1.1.0
                      rust-toml-datetime-0.7.5+spec-1.1.0
                      rust-toml-edit-0.23.10+spec-1.0.0
                      rust-toml-parser-1.0.9+spec-1.1.0
                      rust-toml-writer-1.0.6+spec-1.1.0
                      rust-tracing-0.1.44
                      rust-tracing-attributes-0.1.31
                      rust-tracing-core-0.1.36
                      rust-tracing-log-0.2.0
                      rust-tracing-subscriber-0.3.22
                      rust-tracy-client-0.18.4
                      rust-tracy-client-sys-0.28.0
                      rust-udev-0.9.3
                      rust-unicode-ident-1.0.24
                      rust-unicode-segmentation-1.12.0
                      rust-unicode-xid-0.2.6
                      rust-url-2.5.8
                      rust-utf8-iter-1.0.4
                      rust-utf8parse-0.2.2
                      rust-v-frame-0.3.9
                      rust-valuable-0.1.1
                      rust-version-compare-0.2.1
                      rust-version-check-0.9.5
                      rust-walkdir-2.5.0
                      rust-wasi-0.11.1+wasi-snapshot-preview1
                      rust-wasip2-1.0.1+wasi-0.2.4
                      rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                      rust-wasm-bindgen-0.2.113
                      rust-wasm-bindgen-futures-0.4.63
                      rust-wasm-bindgen-macro-0.2.113
                      rust-wasm-bindgen-macro-support-0.2.113
                      rust-wasm-bindgen-shared-0.2.113
                      rust-wasm-encoder-0.244.0
                      rust-wasm-metadata-0.244.0
                      rust-wasmparser-0.244.0
                      rust-wayland-backend-0.3.15
                      rust-wayland-client-0.31.14
                      rust-wayland-csd-frame-0.3.0
                      rust-wayland-cursor-0.31.14
                      rust-wayland-egl-0.32.11
                      rust-wayland-protocols-0.32.13
                      rust-wayland-protocols-experimental-20251230.0.3
                      rust-wayland-protocols-misc-0.3.12
                      rust-wayland-protocols-plasma-0.3.10
                      rust-wayland-protocols-wlr-0.3.12
                      rust-wayland-scanner-0.31.10
                      rust-wayland-server-0.31.13
                      rust-wayland-sys-0.31.11
                      rust-web-sys-0.3.90
                      rust-web-time-1.1.0
                      rust-weezl-0.1.12
                      rust-which-4.4.2
                      rust-winapi-0.3.9
                      rust-winapi-i686-pc-windows-gnu-0.4.0
                      rust-winapi-util-0.1.11
                      rust-winapi-x86-64-pc-windows-gnu-0.4.0
                      rust-windows-link-0.2.1
                      rust-windows-result-0.4.1
                      rust-windows-sys-0.45.0
                      rust-windows-sys-0.48.0
                      rust-windows-sys-0.59.0
                      rust-windows-sys-0.61.2
                      rust-windows-targets-0.42.2
                      rust-windows-targets-0.48.5
                      rust-windows-targets-0.52.6
                      rust-windows-aarch64-gnullvm-0.42.2
                      rust-windows-aarch64-gnullvm-0.48.5
                      rust-windows-aarch64-gnullvm-0.52.6
                      rust-windows-aarch64-msvc-0.42.2
                      rust-windows-aarch64-msvc-0.48.5
                      rust-windows-aarch64-msvc-0.52.6
                      rust-windows-i686-gnu-0.42.2
                      rust-windows-i686-gnu-0.48.5
                      rust-windows-i686-gnu-0.52.6
                      rust-windows-i686-gnullvm-0.52.6
                      rust-windows-i686-msvc-0.42.2
                      rust-windows-i686-msvc-0.48.5
                      rust-windows-i686-msvc-0.52.6
                      rust-windows-x86-64-gnu-0.42.2
                      rust-windows-x86-64-gnu-0.48.5
                      rust-windows-x86-64-gnu-0.52.6
                      rust-windows-x86-64-gnullvm-0.42.2
                      rust-windows-x86-64-gnullvm-0.48.5
                      rust-windows-x86-64-gnullvm-0.52.6
                      rust-windows-x86-64-msvc-0.42.2
                      rust-windows-x86-64-msvc-0.48.5
                      rust-windows-x86-64-msvc-0.52.6
                      rust-winit-0.31.0-beta.3
                      rust-winit-android-0.31.0-beta.3
                      rust-winit-appkit-0.31.0-beta.3
                      rust-winit-common-0.31.0-beta.3
                      rust-winit-core-0.31.0-beta.3
                      rust-winit-orbital-0.31.0-beta.3
                      rust-winit-uikit-0.31.0-beta.3
                      rust-winit-wayland-0.31.0-beta.3
                      rust-winit-web-0.31.0-beta.3
                      rust-winit-win32-0.31.0-beta.3
                      rust-winit-x11-0.31.0-beta.3
                      rust-winnow-0.7.14
                      rust-wio-0.2.2
                      rust-wit-bindgen-0.46.0
                      rust-wit-bindgen-0.51.0
                      rust-wit-bindgen-core-0.51.0
                      rust-wit-bindgen-rust-0.51.0
                      rust-wit-bindgen-rust-macro-0.51.0
                      rust-wit-component-0.244.0
                      rust-wit-parser-0.244.0
                      rust-wlcs-0.1.0
                      rust-writeable-0.6.4
                      rust-x11-dl-2.21.0
                      rust-x11rb-0.13.2
                      rust-x11rb-protocol-0.13.2
                      rust-xcursor-0.3.10
                      rust-xkbcommon-0.7.0
                      rust-xkbcommon-0.8.0
                      rust-xkbcommon-0.9.0
                      rust-xkbcommon-dl-0.4.2
                      rust-xkeysym-0.2.1
                      rust-xml-rs-0.8.28
                      rust-y4m-0.8.0
                      rust-yoke-0.8.3
                      rust-yoke-derive-0.8.3
                      rust-zerocopy-0.8.39
                      rust-zerocopy-derive-0.8.39
                      rust-zerofrom-0.1.8
                      rust-zerofrom-derive-0.1.8
                      rust-zerotrie-0.2.5
                      rust-zerovec-0.11.8
                      rust-zerovec-derive-0.11.6
                      rust-zmij-1.0.21
                      rust-zune-core-0.4.12
                      rust-zune-core-0.5.1
                      rust-zune-inflate-0.2.54
                      rust-zune-jpeg-0.4.21
                      rust-zune-jpeg-0.5.12)))

emacs-ewm
