(define-module (personal packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

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

(define rust-annotate-snippets-0.9.2
  (crate-source "annotate-snippets" "0.9.2"
                "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-1.0.13
  (crate-source "anstyle" "1.0.13"
                "0y2ynjqajpny6q0amvfzzgw0gfw3l47z85km4gvx87vg02lcr4ji"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.101
  (crate-source "anyhow" "1.0.101"
                "1skmg90fnjnlgs3vl7bksw7036d3rqwqj20n2fxd2ppg67p0y3jz"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-appendlist-1.4.0
  (crate-source "appendlist" "1.4.0"
                "1lnbl7mc7capcqj1z1ylxvm4h492sb9sr8pzww3q6lrhrmrxqjg1"))

(define rust-approx-0.4.0
  (crate-source "approx" "0.4.0"
                "0y52dg58lapl4pp1kqlznfw1blbki0nx6b0aw8kja2yi3gyhaaiz"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

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

(define rust-async-lock-3.4.1
  (crate-source "async-lock" "3.4.1"
                "1p6i1sw3mwv1msdx9jqkr0h0a2jlrp3717yyx5n9pvkw0h23dl2z"))

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

(define rust-atomic-float-1.1.0
  (crate-source "atomic_float" "1.1.0"
                "02j85l9wf0pycq1ad8rwq6h681nk373jqdchwlpvihwaj67j53b2"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-av-scenechange-0.14.1
  (crate-source "av-scenechange" "0.14.1"
                "1543y7riwcy4mmsgcalxcm3bnb41hvwiqiz774nbj68fq9vischg"))

(define rust-av1-grain-0.2.5
  (crate-source "av1-grain" "0.2.5"
                "1y3p43i5xncbny0pfh8kw09am3l3mgyg82ln65r3f434443xpzcc"))

(define rust-avif-serialize-0.8.8
  (crate-source "avif-serialize" "0.8.8"
                "0gd5hr9vd2rkf9gn60f39rham6lzn8a4cdy0p57ihrxx0zq84l1p"))

(define rust-backtrace-0.3.75
  (crate-source "backtrace" "0.3.75"
                "00hhizz29mvd7cdqyz5wrj98vqkihgcxmv2vl7z0d0f53qrac1k8"))

(define rust-base62-2.2.3
  (crate-source "base62" "2.2.3"
                "048i72spz3pvp6n9q4xb8bibb4hsd5qk5pfyjfb4f9vfg1argpqs"))

(define rust-bincode-1.3.3
  (crate-source "bincode" "1.3.3"
                "1bfw3mnwzx5g1465kiqllp5n4r10qrqy88kdlp3jfwnq2ya5xx5i"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

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

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-bitstream-io-4.9.0
  (crate-source "bitstream-io" "4.9.0"
                "0mqpwqy8cqnaqr4wkpk7qy7fjp4x6vxaf8z2hprbvimj3nfvvm30"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.5.1
  (crate-source "block2" "0.5.1"
                "0pyiha5his2grzqr3mynmq244laql2j20992i59asp0gy7mjw4rc"))

(define rust-block2-0.6.1
  (crate-source "block2" "0.6.1"
                "1wnwha7wjjqiamj9abq5l45fyzdxna2k2la0rp9w2hravc5jy39l"))

(define rust-blocking-1.6.2
  (crate-source "blocking" "1.6.2"
                "08bz3f9agqlp3102snkvsll6wc9ag7x5m1xy45ak2rv9pq18sgz8"))

(define rust-bluer-0.17.4
  (crate-source "bluer" "0.17.4"
                "00d9n6gx05kr8yr08nyzcpsmay41961sdvmhr2an86b0bhpi2s5g"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-built-0.8.0
  (crate-source "built" "0.8.0"
                "0r5f08lpjsr6j5ajkbmd0ymfmajpq8ddbfvi8ji8rx48y88qzbgl"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

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

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

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

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-cc-1.2.38
  (crate-source "cc" "1.2.38"
                "1sg7gd94611qhryvb0iip0zibjnhf1yha2wnp0pw2mgrd3himx40"))

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

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-expr-0.20.6
  (crate-source "cfg-expr" "0.20.6"
                "0smbxbd39s2kpmz6r9yg4xmh0wx5d1in6amf49rpr0m6l6szbkkq"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-cgmath-0.18.0
  (crate-source "cgmath" "0.18.0"
                "05sk7c1c1jg5ygqvc3y77kxddp177gwazfibhd864ag3800x760s"))

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
  ;; TODO: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-3.2.25
  (crate-source "clap" "3.2.25"
                "08vi402vfqmfj9f07c4gl6082qxgf4c9x98pbndcnwbgaszq38af"))

(define rust-clap-4.5.60
  (crate-source "clap" "4.5.60"
                "02h3nzznssjgp815nnbzk0r62y2iw03kdli75c233kirld6z75r7"))

(define rust-clap-builder-4.5.60
  (crate-source "clap_builder" "4.5.60"
                "0xk8mdizvmmn6w5ij5cwhy5pbgyac4w9pfvl6nqmjl7a5hql38i4"))

(define rust-clap-derive-3.2.25
  (crate-source "clap_derive" "3.2.25"
                "025hh66cyjk5xhhq8s1qw5wkxvrm8hnv5xwwksax7dy8pnw72qxf"))

(define rust-clap-derive-4.5.55
  (crate-source "clap_derive" "4.5.55"
                "1r949xis3jmhzh387smd70vc8a3b9734ck3g5ahg59a63bd969x9"))

(define rust-clap-lex-0.2.4
  (crate-source "clap_lex" "0.2.4"
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))

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

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-convert-case-0.8.0
  (crate-source "convert_case" "0.8.0"
                "17zqy79xlr1n7nc0n1mlnw5qpp8l2nbxrk13jixrhlavrbna1ams"))

(define rust-cookie-factory-0.3.3
  (crate-source "cookie-factory" "0.3.3"
                "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-core-graphics-0.23.2
  (crate-source "core-graphics" "0.23.2"
                "10dhv3gk4kmbzl14xxkrhhky4fdp8h6nzff6h0019qgr6nz84xy0"))

(define rust-core-graphics-types-0.1.3
  (crate-source "core-graphics-types" "0.1.3"
                "1bxg8nxc8fk4kxnqyanhf36wq0zrjr552c58qy6733zn2ihhwfa5"))

(define rust-core2-0.4.0
  (crate-source "core2" "0.4.0"
                "01f5xv0kf3ds3xm7byg78hycbanb8zlpvsfv4j47y46n3bpsg6xl"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

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

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-ctor-0.1.26
  (crate-source "ctor" "0.1.26"
                "15m0wqhv12p25xkxz5dxvg23r7a6bkh7p8zi1cdhgswjhdl028vd"))

(define rust-cursor-icon-1.2.0
  (crate-source "cursor-icon" "1.2.0"
                "0bvkw7ak1mqwcpkgd9lh7n00hcvlh87jfl7188f231nz6zfy2ypj"))

(define rust-custom-debug-0.6.2
  (crate-source "custom_debug" "0.6.2"
                "13i9cldd9glg8k25z5ll5wb083rnl07pl7bzhwgf3cv7jnnx39rd"))

(define rust-custom-debug-derive-0.6.2
  (crate-source "custom_debug_derive" "0.6.2"
                "04cw7wqf42rjnah5b79vpwawh94m5rjjbrrb9xicgxjjhvdcw1x7"))

(define rust-darling-0.10.2
  (crate-source "darling" "0.10.2"
                "0n7qsp6854wm3y1q1lvylhv15zvc87ibbac1nyfmcdbyv1snww0d"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.10.2
  (crate-source "darling_core" "0.10.2"
                "16sija1jv0l754x4aa6b6fy01d1kf8m0r4id3flqipm45np61jgh"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-macro-0.10.2
  (crate-source "darling_macro" "0.10.2"
                "0wlv31cxkrjijz5gv13hvk55c9lmd781aj12c8n84sa9mksa5dfr"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-dbus-0.9.9
  (crate-source "dbus" "0.9.9"
                "1sfib87472q429k3j1hwhbjc7vcpjhz8hnnzd2ssfmdbx1an42qr"))

(define rust-dbus-crossroads-0.5.2
  (crate-source "dbus-crossroads" "0.5.2"
                "1q3dyywazr3hppm052fa8q2366q66ml789r42jjlnm47f51q6k1s"))

(define rust-dbus-tokio-0.7.6
  (crate-source "dbus-tokio" "0.7.6"
                "04xd3z2dnjv4d45kj3wqnwbnwllrp1zsg8v3q0qp2rxwb7a8hxh0"))

(define rust-deranged-0.5.3
  (crate-source "deranged" "0.5.3"
                "1k473y8lzjac956dm3s1cs2rz364py9zd52y9fkbanws8b6vqc6n"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dispatch-0.2.0
  (crate-source "dispatch" "0.2.0"
                "0fwjr9b7582ic5689zxj8lf7zl94iklhlns3yivrnv8c9fxr635x"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-dpi-0.1.2
  (crate-source "dpi" "0.1.2"
                "0xhsvzgjvdch2fwmfc9vkb708b0q59b6imypyjlgbiigyb74rcfq"))

(define rust-drm-0.14.1
  (crate-source "drm" "0.14.1"
                "0vvmj9n0wslrbw3rinpzlfyhwwgr02gqspy1al5gfh99dif8rg40"))

(define rust-drm-ffi-0.9.0
  (crate-source "drm-ffi" "0.9.0"
                "12vff80hdpp81gj5lqw25xnkppwsxc4wklpn8nc556wsv5ci9r6q"))

(define rust-drm-fourcc-2.2.0
  (crate-source "drm-fourcc" "2.2.0"
                "1x76v9a0pkgym4n6cah4barnai9gsssm7gjzxskw2agwibdvrbqa"))

(define rust-drm-sys-0.8.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "drm-sys" "0.8.0"
                "1345z72hd2rna4qxd2zcpbzvw0z7ywfndk6g2ngdci69vg46dyxs"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-emacs-0.19.0
  (crate-source "emacs" "0.19.0"
                "133ip19m1nq8krf04klh1jpm833wpw6kdy2jksgylbw4xcp3yxws"))

(define rust-emacs-macros-0.17.0
  (crate-source "emacs-macros" "0.17.0"
                "0qg1dcn5acbirq617qq2fgg9adswif2dnr292s3qnq62wzgnyrb9"))

(define rust-emacs-module-0.18.0
  (crate-source "emacs_module" "0.18.0"
                "1ypjyyv2ca3vza4sia91ckxamgfk63yd8frkvg3d4ph4fk4pn1mk"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-encoding-rs-0.8.35
  (crate-source "encoding_rs" "0.8.35"
                "1wv64xdrr9v37rqqdjsyb8l8wzlcbab80ryxhrszvnj59wy0y0vm"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-endi-1.1.1
  (crate-source "endi" "1.1.1"
                "16a0076dx41vgrzzimm9clcym77h732czqjiajanmzvd1i1y5dv6"))

(define rust-enumflags2-0.7.12
  (crate-source "enumflags2" "0.7.12"
                "1vzcskg4dca2jiflsfx1p9yw1fvgzcakcs7cpip0agl51ilgf9qh"))

(define rust-enumflags2-derive-0.7.12
  (crate-source "enumflags2_derive" "0.7.12"
                "09rqffacafl1b83ir55hrah9gza0x7pzjn6lr6jm76fzix6qmiv7"))

(define rust-env-filter-0.1.3
  (crate-source "env_filter" "0.1.3"
                "1l4p6f845cylripc3zkxa0lklk8rn2q86fqm522p6l2cknjhavhq"))

(define rust-env-logger-0.11.8
  (crate-source "env_logger" "0.11.8"
                "17q6zbjam4wq75fa3m4gvvmv3rj3ch25abwbm84b28a0j3q67j0k"))

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

(define rust-fax-0.2.6
  (crate-source "fax" "0.2.6"
                "1ax0jmvsszxd03hj6ga1kyl7gaqcfw0akg2wf0q6gk9pizaffpgh"))

(define rust-fax-derive-0.2.0
  (crate-source "fax_derive" "0.2.0"
                "0zap434zz4xvi5rnysmwzzivig593b4ng15vwzwl7js2nw7s3b50"))

(define rust-fdeflate-0.3.7
  (crate-source "fdeflate" "0.3.7"
                "130ga18vyxbb5idbgi07njymdaavvk6j08yh1dfarm294ssm6s0y"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

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

(define rust-foreign-types-0.5.0
  (crate-source "foreign-types" "0.5.0"
                "0rfr2zfxnx9rz3292z5nyk8qs2iirznn5ff3rd4vgdwza6mdjdyp"))

(define rust-foreign-types-macros-0.2.3
  (crate-source "foreign-types-macros" "0.2.3"
                "0hjpii8ny6l7h7jpns2cp9589016l8mlrpaigcnayjn9bdc6qp0s"))

(define rust-foreign-types-shared-0.3.1
  (crate-source "foreign-types-shared" "0.3.1"
                "0nykdvv41a3d4py61bylmlwjhhvdm0b3bcj9vxhqgxaxnp5ik6ma"))

(define rust-fps-ticker-1.0.0
  (crate-source "fps_ticker" "1.0.0"
                "06cj5c5rk5grm2ajh4sabcppxr1h57gxfqacvi5psxb9zw2lj5py"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

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

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

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

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-gethostname-1.1.0
  (crate-source "gethostname" "1.1.0"
                "1n6bj9gh503ggjblfjcai96gmxynxsrykaynljlrfdra34q95m0v"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-gif-0.14.1
  (crate-source "gif" "0.14.1"
                "0pn3ldqjk0ng1vbc3r3zqqrnjkn6s3f3ndk96lhhrn0q82l2ppzm"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

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

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.8.1
  (crate-source "globwalk" "0.8.1"
                "1k6xwkydr7igvwjn3xkwjywk4213lcs53f576ilqz1h84jaazqwk"))

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

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

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

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-image-0.25.9
  (crate-source "image" "0.25.9"
                "06lwa4ag3zcmjzivl356q0qhgxxqpkp7qwda7x0mjrkq21n6ql76"))

(define rust-image-webp-0.2.4
  (crate-source "image-webp" "0.2.4"
                "1hz814csyi9283vinzlkix6qpnd6hs3fkw7xl6z2zgm4w7rrypjj"))

(define rust-imgref-1.12.0
  (crate-source "imgref" "1.12.0"
                "1j3iwdal9mdkmyrsms3lz4n1bxxxjxss2jvbmh662fns63fcxig7"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-input-0.9.1
  (crate-source "input" "0.9.1"
                "1abmv1djhynihipjppgsmw6nbp6pcgzk8rzi4v6wmyci9990kp7v"))

(define rust-input-sys-1.18.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "input-sys" "1.18.0"
                "1c4y24wf0jixi52js4f7cjspbgi0bzzaqfhn8m91qcq03i6mnkxx"))

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

(define rust-io-uring-0.7.10
  (crate-source "io-uring" "0.7.10"
                "0yvjyygwdcqjcgw8zp254hvjbm7as1c075dl50spdshas3aa4vq4"))

(define rust-is-terminal-0.4.17
  (crate-source "is-terminal" "0.4.17"
                "0ilfr9n31m0k6fsm3gvfrqaa62kbzkjqpwcd9mc46klfig1w2h1n"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-iwdrs-0.1.6
  (crate-source "iwdrs" "0.1.6"
                "13yc1jbw43hgh7gwyx2yj47j6bfn59h8a7dpyr00pr1djl9kf5x1"))

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

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

(define rust-js-sys-0.3.80
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.80"
                "0bkhnbna0a9sqhhswfar0mzi8mpy2dygv4zbzfdbm97bqnz16bw5"))

(define rust-js-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-js-sys-0.3.90
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.90"
                "19m5qg024y5xanjrq5c6m1sx69nnzqw7ychnbgnx9xmka1j6zp0l"))

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

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.180
  (crate-source "libc" "0.2.180"
                "1z2n7hl10fnk1xnv19ahhqxwnb4qi9aclnl6gigim2aaahw5mhxw"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

(define rust-libdbus-sys-0.2.6
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.6"
                "17xx4dy30fn81zhwsm4y2c84wr0apyiams8hy20lc3mmzrp8bgjw"))

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

(define rust-libredox-0.1.12
  (crate-source "libredox" "0.1.12"
                "05h6fb2y05h74zwaafmnf7gv3bxilzp7syqlfzw524w55kh9a2rx"))

(define rust-libseat-0.2.4
  (crate-source "libseat" "0.2.4"
                "0cggn682xklm5h7i8bbjc48wjpys9wz2y8xa7ywgyrh3dsdwcmk6"))

(define rust-libseat-sys-0.2.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libseat-sys" "0.2.0"
                "1yvx15lx8qj3xycdx4ddzs681ayhg5vpdvgzsfl64pxy93x89978"))

(define rust-libspa-0.8.0
  (crate-source "libspa" "0.8.0"
                "044qs48yl0llp2dmrgwxj9y1pgfy09i6fhq661zqqb9a3fwa9wv5"))

(define rust-libspa-0.9.2
  (crate-source "libspa" "0.9.2"
                "1x0dq254f60vva671css7mkwsfj357wrwsrcr6s2frk5lyiczf5n"))

(define rust-libspa-sys-0.8.0
  ;; TODO: Check bundled sources.
  (crate-source "libspa-sys" "0.8.0"
                "07yh4i5grzbxkchg6dnxlwbdw2wm5jnd7ffbhl77jr0388b9f3dz"))

(define rust-libspa-sys-0.9.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libspa-sys" "0.9.2"
                "1q66vim2wha1rdglqn5w0i42z59pa9s5s8sqj37xxdifbm2lj44h"))

(define rust-libudev-sys-0.1.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libudev-sys" "0.1.4"
                "09236fdzlx9l0dlrsc6xx21v5x8flpfm3d5rjq9jr5ivlas6k11w"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
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

(define rust-linux-raw-sys-0.6.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.6.5"
                "1mv3c1zz51ydcj768zavm8g06gz5jb1p7yigmmif7hz5whdmnf1a"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

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

(define rust-mac-notification-sys-0.6.6
  ;; TODO: Check bundled sources.
  (crate-source "mac-notification-sys" "0.6.6"
                "1m0wl0rprmrjbv47h0vwff3a2isq8wkddagdr521pxja1288970i"))

(define rust-macaddr-1.0.1
  (crate-source "macaddr" "1.0.1"
                "1n5jxn79krlql810c4w3hdkvyqc01141dc5y6fr9sxff2yy0pvms"))

(define rust-matchers-0.1.0
  (crate-source "matchers" "0.1.0"
                "0n2mbk7lg2vf962c8xwzdq96yrc9i0p8dbmm4wa1nnkcp1dhfqw2"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-maybe-rayon-0.1.1
  (crate-source "maybe-rayon" "0.1.1"
                "06cmvhj4n36459g327ng5dnj8d58qs472pv5ahlhm7ynxl6g78cf"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

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

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

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

(define rust-nix-0.29.0
  (crate-source "nix" "0.29.0"
                "0ikvn7s9r2lrfdm3mx1h7nbfjvcc6s9vxdzw7j5xfkd2qdnp9qki"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-nom-8.0.0
  (crate-source "nom" "8.0.0"
                "01cl5xng9d0gxf26h39m0l8lprgpa00fcc75ps1yzgbib1vn35yz"))

(define rust-noop-proc-macro-0.3.0
  (crate-source "noop_proc_macro" "0.3.0"
                "1j2v1c6ric4w9v12h34jghzmngcwmn0hll1ywly4h6lcm4rbnxh6"))

(define rust-normpath-1.5.0
  (crate-source "normpath" "1.5.0"
                "16z68q809749ky2vl72f3lqnhf3vjclvcc3y2z5v8m2nj0msn8xz"))

(define rust-notify-rust-4.11.7
  (crate-source "notify-rust" "4.11.7"
                "0024xqbn29z1k6cfbi8w7c1p73hscqwkpbwlwwa2bam5cn328hk4"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-bigint-0.4.6
  (crate-source "num-bigint" "0.4.6"
                "1f903zd33i6hkjpsgwhqwi2wffnvkxbn6rv4mkgcjcqi7xr4zr55"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

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

(define rust-objc-sys-0.3.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "objc-sys" "0.3.5"
                "0423gry7s3rmz8s3pzzm1zy5mdjif75g6dbzc2lf2z0c77fipffd"))

(define rust-objc2-0.5.2
  (crate-source "objc2" "0.5.2"
                "015qa2d3vh7c1j2736h5wjrznri7x5ic35vl916c22gzxva8b9s6"))

(define rust-objc2-0.6.2
  (crate-source "objc2" "0.6.2"
                "1g3qa1vxp6nlh4wllll921z299d3s1is31m1ccasd8pklxxka7sn"))

(define rust-objc2-app-kit-0.2.2
  (crate-source "objc2-app-kit" "0.2.2"
                "1zqyi5l1bm26j1bgmac9783ah36m5kcrxlqp5carglnpwgcrms74"))

(define rust-objc2-cloud-kit-0.2.2
  (crate-source "objc2-cloud-kit" "0.2.2"
                "02dhjvmcq8c2bwj31jx423jygif1scs9f0lmlab0ayhw75b3ppbl"))

(define rust-objc2-contacts-0.2.2
  (crate-source "objc2-contacts" "0.2.2"
                "12a8m927xrrxa54xhqhqnkkl1a6l07pyrpnqfk9jz09kkh755zx5"))

(define rust-objc2-core-data-0.2.2
  (crate-source "objc2-core-data" "0.2.2"
                "1vvk8zjylfjjj04dzawydmqqz5ajvdkhf22cnb07ihbiw14vyzv1"))

(define rust-objc2-core-foundation-0.3.1
  (crate-source "objc2-core-foundation" "0.3.1"
                "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))

(define rust-objc2-core-image-0.2.2
  (crate-source "objc2-core-image" "0.2.2"
                "102csfb82zi2sbzliwsfd589ckz0gysf7y6434c9zj97lmihj9jm"))

(define rust-objc2-core-location-0.2.2
  (crate-source "objc2-core-location" "0.2.2"
                "10apgsrigqryvi4rcc0f6yfjflvrl83f4bi5hkr48ck89vizw300"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.2.2
  (crate-source "objc2-foundation" "0.2.2"
                "1a6mi77jsig7950vmx9ydvsxaighzdiglk5d229k569pvajkirhf"))

(define rust-objc2-foundation-0.3.1
  (crate-source "objc2-foundation" "0.3.1"
                "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))

(define rust-objc2-link-presentation-0.2.2
  (crate-source "objc2-link-presentation" "0.2.2"
                "160k4qh00yrx57dabn3hzas4r98kmk9bc0qsy1jvwday3irax8d1"))

(define rust-objc2-metal-0.2.2
  (crate-source "objc2-metal" "0.2.2"
                "1mmdga66qpxrcfq3gxxhysfx3zg1hpx4z886liv3j0pnfq9bl36x"))

(define rust-objc2-quartz-core-0.2.2
  (crate-source "objc2-quartz-core" "0.2.2"
                "0ynw8819c36l11rim8n0yzk0fskbzrgaqayscyqi8swhzxxywaz4"))

(define rust-objc2-symbols-0.2.2
  (crate-source "objc2-symbols" "0.2.2"
                "1p04hjkxan18g2b7h9n2n8xxsvazapv2h6mfmmdk06zc7pz4ws0a"))

(define rust-objc2-ui-kit-0.2.2
  (crate-source "objc2-ui-kit" "0.2.2"
                "0vrb5r8z658l8c19bx78qks8c5hg956544yirf8npk90idwldfxq"))

(define rust-objc2-uniform-type-identifiers-0.2.2
  (crate-source "objc2-uniform-type-identifiers" "0.2.2"
                "1ziv4wkbxcaw015ypg0q49ycl7m14l3x56mpq2k1rznv92bmzyj4"))

(define rust-objc2-user-notifications-0.2.2
  (crate-source "objc2-user-notifications" "0.2.2"
                "1cscv2w3vxzaslz101ddv0z9ycrrs4ayikk4my4qd3im8bvcpkvn"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-orbclient-0.3.50
  (crate-source "orbclient" "0.3.50"
                "1p8z477y3f7258gabbnv145ih71svndwac6cs6jpl2vhmrmjrbaj"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

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

(define rust-pipewire-0.8.0
  (crate-source "pipewire" "0.8.0"
                "1nldg1hz4v0qr26lzdxqpvrac4zbc3pb6436sl392425bjx4brh8"))

(define rust-pipewire-0.9.2
  (crate-source "pipewire" "0.9.2"
                "0i4ddb89cr8x02zqy35krlx5mgkd3mqr0qbwkx4mdmqipydbi24n"))

(define rust-pipewire-sys-0.8.0
  ;; TODO: Check bundled sources.
  (crate-source "pipewire-sys" "0.8.0"
                "04hiy3rl8v3j2dfzp04gr7r8l5azzqqsvqdzwa7sipdij27ii7l4"))

(define rust-pipewire-sys-0.9.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "pipewire-sys" "0.9.2"
                "0dpa8q10b9cja5z5r5zgb8q27pnpla7kn3h91c11gjnnw3z8l0nb"))

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

(define rust-portable-atomic-1.11.1
  (crate-source "portable-atomic" "1.11.1"
                "10s4cx9y3jvw0idip09ar52s2kymq8rq9a668f793shn1ar6fhpq"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro-error-1.0.4
  (crate-source "proc-macro-error" "1.0.4"
                "1373bhxaf0pagd8zkyd03kkx6bchzf6g0dkwrwzsnal9z47lj9fs"))

(define rust-proc-macro-error-attr-1.0.4
  (crate-source "proc-macro-error-attr" "1.0.4"
                "0sgq6m5jfmasmwwy8x4mjygx5l7kp8s4j60bv25ckv2j1qc41gm1"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-process-wrap-8.2.1
  (crate-source "process-wrap" "8.2.1"
                "189vzjn8dan18cnb0qlk3b472a6imji8wqlzxj13mwi20hplzvx3"))

(define rust-profiling-1.0.17
  (crate-source "profiling" "1.0.17"
                "0wqp6i1bl7azy9270dp92srbbr55mgdh9qnk5b1y44lyarmlif1y"))

(define rust-profiling-procmacros-1.0.17
  (crate-source "profiling-procmacros" "1.0.17"
                "0nrxdh5r723raxbs136jmjx46p0c5qgai8jwz4j555mn0ad7ywaj"))

(define rust-proptest-1.10.0
  (crate-source "proptest" "1.10.0"
                "0ch5r381al5z7089j47gkyybzbgygkgld5bzfg019vxcznrnqmip"))

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

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quick-xml-0.38.4
  (crate-source "quick-xml" "0.38.4"
                "0772siy4d9vlq77842012c8cycs3y0szxkv62rh9sh2sqmc20v5n"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-rand-0.8.5
  (crate-source "rand" "0.8.5"
                "013l6931nn7gkc23jz5mm3qdhf93jjf0fg64nz2lp4i51qd8vbrl"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.3.1
  (crate-source "rand_chacha" "0.3.1"
                "123x2adin558xbhvqb8w4f6syjsdkmqff8cxwhmjacpsl1ihmhg6"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

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

(define rust-redox-syscall-0.4.1
  (crate-source "redox_syscall" "0.4.1"
                "1aiifyz5dnybfvkk4cdab9p2kmphag1yad6iknc7aszlxxldf8j7"))

(define rust-redox-syscall-0.5.18
  (crate-source "redox_syscall" "0.5.18"
                "0b9n38zsxylql36vybw18if68yc9jczxmbyzdwyhb9sifmag4azd"))

(define rust-redox-syscall-0.7.2
  (crate-source "redox_syscall" "0.7.2"
                "1d54z7ydfhwpp6c93mz5liwx12ykvx82p2yc0bfd8cnrghpxv53d"))

(define rust-regex-1.11.2
  (crate-source "regex" "1.11.2"
                "04k9rzxd11hcahpyihlswy6f1zqw7lspirv4imm4h0lcdl8gvmr3"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.1.10
  (crate-source "regex-automata" "0.1.10"
                "0ci1hvbzhrfby5fdpf4ganhf7kla58acad9i1ff1p34dzdrhs8vc"))

(define rust-regex-automata-0.4.10
  (crate-source "regex-automata" "0.4.10"
                "1mllcfmgjcl6d52d5k09lwwq9wj5mwxccix4bhmw5spy1gx5i53b"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-syntax-0.6.29
  (crate-source "regex-syntax" "0.6.29"
                "1qgj49vm6y3zn1hi09x91jvgkl2b1fiaq402skj83280ggfwcqpi"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-regex-syntax-0.8.9
  (crate-source "regex-syntax" "0.8.9"
                "0k0a47r1rcl794wj8a948niakbg081s5pp5nlgcbmmr2iy3qfs59"))

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

(define rust-rust-i18n-3.1.5
  (crate-source "rust-i18n" "3.1.5"
                "0cn3gmqhabcnskv81krmcnp94yq4azhibp1s53pcav5gvwgmb8px"))

(define rust-rust-i18n-macro-3.1.5
  (crate-source "rust-i18n-macro" "3.1.5"
                "0rgra2j2ynrbd4z5072z5b72n3fllmbvpxi4pqzd4mk6ypbzgfi2"))

(define rust-rust-i18n-support-3.1.5
  (crate-source "rust-i18n-support" "3.1.5"
                "06cgfn6vad512vq4lf6l0ybascvi7db1wxvd0m902k5s5gsx83ll"))

(define rust-rustc-demangle-0.1.26
  (crate-source "rustc-demangle" "0.1.26"
                "1kja3nb0yhlm4j2p1hl8d7sjmn2g9fa1s4pj0qma5kj2lcndkxsn"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.2.3
  (crate-source "rustc_version" "0.2.3"
                "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

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

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

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

(define rust-serde-1.0.226
  (crate-source "serde" "1.0.226"
                "1zcm2asp9fiphbp0k96whabw02kiiqgzxhbyz85vc92v088n9jhd"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.226
  (crate-source "serde_core" "1.0.226"
                "1936x6cpqgyq57nm7qi416dsc4fiq3jv6d7vh74xmfgdk4wscaxs"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.226
  (crate-source "serde_derive" "1.0.226"
                "0cyvkilp34an3f90b0idw0jjsyq20h7v47gsp8qkfmrl5zi3mdcd"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.4
  (crate-source "serde_spanned" "1.0.4"
                "0xkp0qdzams5sqwndbw3xrhf4c0bb5r46w2ywkp1aqsdb8ggkfzq"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

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

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-slotmap-1.1.1
  (crate-source "slotmap" "1.1.1"
                "0f20xf53zaysx9ydzkwwqm6hsjyb8lj2j6amhg57iln3jcy8rmdx"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-smithay-0.7.0.ae14fa1
  ;; TODO REVIEW: Define standalone package if this is a workspace.
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/Smithay/smithay.git")
                        (commit "ae14fa12f6ee3fc4dd4c766ffb21848f991a6a18")))
    (file-name (git-file-name "rust-smithay" "0.7.0.ae14fa1"))
    (sha256 (base32 "0a56387p9s5qsy7m9br63frssswbfjgyiwmwf1msj7lq56ms0bq7"))))

(define rust-smithay-client-toolkit-0.19.2
  (crate-source "smithay-client-toolkit" "0.19.2"
                "05h05hg4dn3v6br5jbdbs5nalk076a64s7fn6i01nqzby2hxwmrl"))

(define-syntax-rule (my-cargo-inputs name)
  (cargo-inputs name #:module '(personal packages rust-crates)))

(define rust-smithay-drm-extras-0.1.0.ae14fa1
  (let ((commit "ae14fa12f6ee3fc4dd4c766ffb21848f991a6a18"))
    (hidden-package
     (package
       (inherit rust-smithay-0.6.0.ede2707)
       (name "rust-smithay")
       (version (git-version "0.7.0" "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/Smithay/smithay")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0a56387p9s5qsy7m9br63frssswbfjgyiwmwf1msj7lq56ms0bq7"))))
       (inputs (my-cargo-inputs 'rust-smithay-0.7.0.ae14fa1))))))

(define rust-smol-str-0.2.2
  (crate-source "smol_str" "0.2.2"
                "1bfylqf2vnqaglw58930vpxm2rfzji5gjp15a2c0kh8aj6v8ylyx"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strsim-0.9.3
  (crate-source "strsim" "0.9.3"
                "0k497pv882qn3q977ckznm13vxx927g8s1swvcv68j3c1pccwik4"))

(define rust-strum-0.26.3
  (crate-source "strum" "0.26.3"
                "01lgl6jvrf4j28v5kmx9bp480ygf1nhvac8b4p7rcj9hxw50zv4g"))

(define rust-strum-macros-0.26.4
  (crate-source "strum_macros" "0.26.4"
                "1gl1wmq24b8md527cpyd5bw9rkbqldd7k1h38kf5ajd2ln2ywssc"))

(define rust-syn-1.0.109
  (crate-source "syn" "1.0.109"
                "0ds2if4600bd59wsv7jjgfkayfzy3hnazs394kz6zdkmna8l3dkj"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.114
  (crate-source "syn" "2.0.114"
                "0akw62dizhyrkf3ym1jsys0gy1nphzgv0y8qkgpi6c1s4vghglfl"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-system-deps-7.0.7
  (crate-source "system-deps" "7.0.7"
                "0zsyh2m893nqkp1wri5c85favp2xyl1qpjxnd5nz31pr6qvz7j28"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-target-lexicon-0.13.3
  (crate-source "target-lexicon" "0.13.3"
                "0355pbycq0cj29h1rp176l57qnfwmygv7hwzchs7iq15gibn4zyz"))

(define rust-tauri-winrt-notification-0.7.2
  (crate-source "tauri-winrt-notification" "0.7.2"
                "1fd9gcllx1rkp9h1ppq976bhqppnil5xsy36li1zx2g4gph6c7hb"))

(define rust-tempfile-3.22.0
  (crate-source "tempfile" "3.22.0"
                "0lza9r7dzm4k9fghw24yql6iz59wq8xgs46a7i29ir6xz88lvyl4"))

(define rust-tempfile-3.24.0
  (crate-source "tempfile" "3.24.0"
                "171fz3h6rj676miq15fyv1hnv69p426mlp8489bwa1b3xg3sjpb5"))

(define rust-tempfile-3.26.0
  (crate-source "tempfile" "3.26.0"
                "182lfcv9d5w9349i0rjlgn4431k2m3yqfn9ls84p9d3ifxv2r9w2"))

(define rust-termcolor-1.4.1
  (crate-source "termcolor" "1.4.1"
                "0mappjh3fj3p2nmrg4y7qv94rchwi9mzmgmfflr8p2awdj7lyy86"))

(define rust-textwrap-0.16.2
  (crate-source "textwrap" "0.16.2"
                "0mrhd8q0dnh5hwbwhiv89c6i41yzmhw4clwa592rrp24b9hlfdf1"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-tiff-0.10.3
  (crate-source "tiff" "0.10.3"
                "0vrkdk9cdk07rh7iifcxpn6m8zv3wz695mizhr8rb3gfgzg0b5mg"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tokio-1.47.1
  (crate-source "tokio" "1.47.1"
                "0f2hp5v3payg6x04ijj67si1wsdhksskhmjs2k9p5f7bmpyrmr49"))

(define rust-tokio-macros-2.5.0
  (crate-source "tokio-macros" "2.5.0"
                "1f6az2xbvqp7am417b78d1za8axbvjvxnmkakz9vr8s52czx81kf"))

(define rust-tokio-stream-0.1.17
  (crate-source "tokio-stream" "0.1.17"
                "0ix0770hfp4x5rh5bl7vsnr3d4iz4ms43i522xw70xaap9xqv9gc"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.11+spec-1.1.0
  (crate-source "toml" "0.9.11+spec-1.1.0"
                "0ikwmd5s9ndg6afxijaxjcgxw53sd9af3mmfzymf37rh92lckbzk"))

(define rust-toml-0.9.12+spec-1.1.0
  (crate-source "toml" "0.9.12+spec-1.1.0"
                "0qwqbrymqn88mg2yqyq3rj52z6p20448z0jxdbpjsbpwg5g894ng"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.23.10+spec-1.0.0
  (crate-source "toml_edit" "0.23.10+spec-1.0.0"
                "0saj5c676j8a3sqaj9akkp09wambg8aflji4zblwwa70azvvkj44"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-parser-1.0.6+spec-1.1.0
  (crate-source "toml_parser" "1.0.6+spec-1.1.0"
                "0i5zxv5y3z9g6r3gm6ly4q0hhkahh013q4rys2fz04cf195qn6d3"))

(define rust-toml-parser-1.0.9+spec-1.1.0
  (crate-source "toml_parser" "1.0.9+spec-1.1.0"
                "1i54qpvvcppy8ybdn9gssas81vfzq0kmgkcnxzhyf8w9w0al8bbh"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-toml-writer-1.0.6+spec-1.1.0
  (crate-source "toml_writer" "1.0.6+spec-1.1.0"
                "01r6x42d1p8p5kzfsi1fm4dakm3w53vi69f2ivyqpvi1xm5g25mb"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

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

(define rust-triomphe-0.1.14
  (crate-source "triomphe" "0.1.14"
                "11fciha522hrz6pkafy3xlq20w405w9dqvy9ln7ba1s8v8k7g3zg"))

(define rust-typenum-1.19.0
  (crate-source "typenum" "1.19.0"
                "1fw2mpbn2vmqan56j1b3fbpcdg80mz26fm53fs16bq5xcq84hban"))

(define rust-udev-0.9.3
  (crate-source "udev" "0.9.3"
                "17vy1yc6ipb5m2kc2d4lx2qpj45yr7grsjzm3y2gq0a4xblkfkmg"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unarray-0.1.4
  (crate-source "unarray" "0.1.4"
                "154smf048k84prsdgh09nkm2n0w0336v84jd4zikyn6v6jrqbspa"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-ident-1.0.22
  (crate-source "unicode-ident" "1.0.22"
                "1x8xrz17vqi6qmkkcqr8cyf0an76ig7390j9cnqnk47zyv2gf4lk"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

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

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

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

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.103
  (crate-source "wasm-bindgen" "0.2.103"
                "069qhf7yrl4jymzjzvwsmcmw96al639xim4scigpy5qapngsc45b"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-0.2.113
  (crate-source "wasm-bindgen" "0.2.113"
                "1wpg101a5rqqilv4cz4929kbph9g15y4v2fvkbg7yjsrgy9jlwk0"))

(define rust-wasm-bindgen-backend-0.2.103
  (crate-source "wasm-bindgen-backend" "0.2.103"
                "070x7fjnnvzm2y3a5j29wmss4z547cjdx3rnpixh19j56m105dqb"))

(define rust-wasm-bindgen-futures-0.4.63
  (crate-source "wasm-bindgen-futures" "0.4.63"
                "06j4hyxvlfvas7lhvgai44vh8izd59774wv5m8hla3kp1djz92ca"))

(define rust-wasm-bindgen-macro-0.2.103
  (crate-source "wasm-bindgen-macro" "0.2.103"
                "18481jkmczv4j0m747ypb0k1acq093hhbdhpb4sr856r27sg8rgw"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-0.2.113
  (crate-source "wasm-bindgen-macro" "0.2.113"
                "0l1rbylzb1cs5i6ihmkgk8zic71pg563yadgqj8nnjq9jmiqrb0g"))

(define rust-wasm-bindgen-macro-support-0.2.103
  (crate-source "wasm-bindgen-macro-support" "0.2.103"
                "0clsx611pday95s6wg8pndvrd8xknsaf20d40kk8x2irj6lh7h7z"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-macro-support-0.2.113
  (crate-source "wasm-bindgen-macro-support" "0.2.113"
                "0q4xmjmq1c80drv84hz9i9l7fj3yi0v2d11kh1r21p2rc77angxb"))

(define rust-wasm-bindgen-shared-0.2.103
  (crate-source "wasm-bindgen-shared" "0.2.103"
                "1kx13fvmlxxaxf04vm3b14437hyq92zdy89pvcaclc54xzs3fg19"))

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

(define rust-wayland-backend-0.3.12
  (crate-source "wayland-backend" "0.3.12"
                "1yb4s5mbcis3z3gcmxq2lzgrcw2li7jsfr9ayi4gcsyrrja43rpy"))

(define rust-wayland-client-0.31.12
  (crate-source "wayland-client" "0.31.12"
                "1v1b2b2s0ld41psn3v2p3c6i590iz3r427czrf3c3dpv6yjzmrmq"))

(define rust-wayland-csd-frame-0.3.0
  (crate-source "wayland-csd-frame" "0.3.0"
                "0zjcmcqprfzx57hlm741n89ssp4sha5yh5cnmbk2agflvclm0p32"))

(define rust-wayland-cursor-0.31.12
  (crate-source "wayland-cursor" "0.31.12"
                "0y5hzl8z0da7sa2fg877bwl5mi56k15askmpx2qhcjq6nssw8r2q"))

(define rust-wayland-egl-0.32.9
  (crate-source "wayland-egl" "0.32.9"
                "0m78bnlgrykkc3ahbmy5xvc4y1ky3vf7b3lxf3ai2bivbzrrwb4a"))

(define rust-wayland-protocols-0.32.10
  (crate-source "wayland-protocols" "0.32.10"
                "1wzl7ly3ahi2y4swf8wmlqaj3gck4fpmwf6ymbfxd37wpkzskvds"))

(define rust-wayland-protocols-misc-0.3.10
  (crate-source "wayland-protocols-misc" "0.3.10"
                "0kn3nk770vf29d227d8r88rpzr6i4x9q3pb9f6inlh65xvymh73r"))

(define rust-wayland-protocols-plasma-0.3.10
  (crate-source "wayland-protocols-plasma" "0.3.10"
                "0srh0xhf4jpz8s1kx531lh0ixzx5v6p3iwwpk9d562ih3536765a"))

(define rust-wayland-protocols-wlr-0.3.10
  (crate-source "wayland-protocols-wlr" "0.3.10"
                "1ws5fd7qs5vf3digbnn20n7mks2sdg76sy13b36k836g0bgpqng9"))

(define rust-wayland-scanner-0.31.8
  (crate-source "wayland-scanner" "0.31.8"
                "1qw971z9jcxdw8s371gx2anmwb95m59y38q3k11qxrk3d95yj8sl"))

(define rust-wayland-server-0.31.11
  (crate-source "wayland-server" "0.31.11"
                "1j8qn606nabs6xg4hd41kfjyr4123ddmqi9n3mqrgxfiz28ap5wj"))

(define rust-wayland-sys-0.31.8
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "wayland-sys" "0.31.8"
                "1zdxrcl8paklwir0lag1i80k6h0iq1f80d925b4p9yaymk1vyv8y"))

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

(define rust-windows-0.61.3
  (crate-source "windows" "0.61.3"
                "14v8dln7i4ccskd8danzri22bkjkbmgzh284j3vaxhd4cykx7awv"))

(define rust-windows-aarch64-gnullvm-0.42.2
  (crate-source "windows_aarch64_gnullvm" "0.42.2"
                "1y4q0qmvl0lvp7syxvfykafvmwal5hrjb4fmv04bqs0bawc52yjr"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.42.2
  (crate-source "windows_aarch64_msvc" "0.42.2"
                "0hsdikjl5sa1fva5qskpwlxzpc5q9l909fpl1w6yy1hglrj8i3p0"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-future-0.2.1
  (crate-source "windows-future" "0.2.1"
                "13mdzcdn51ckpzp3frb8glnmkyjr1c30ym9wnzj9zc97hkll2spw"))

(define rust-windows-i686-gnu-0.42.2
  (crate-source "windows_i686_gnu" "0.42.2"
                "0kx866dfrby88lqs9v1vgmrkk1z6af9lhaghh5maj7d4imyr47f6"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.42.2
  (crate-source "windows_i686_msvc" "0.42.2"
                "0q0h9m2aq1pygc199pa5jgc952qhcnf0zn688454i7v4xjv41n24"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-sys-0.45.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.45.0"
                "1l36bcqm4g89pknfp8r9rl1w4bn017q6a8qlx8viv0xjxzjkna3m"))

(define rust-windows-sys-0.48.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.0"
                "1ajpwsmzfcsa1r7i0dxzvfn24dp3525rcd7aq95ydvdj8171h0g2"))

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

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-threading-0.1.0
  (crate-source "windows-threading" "0.1.0"
                "19jpn37zpjj2q7pn07dpq0ay300w65qx7wdp13wbp8qf5snn6r5n"))

(define rust-windows-version-0.1.5
  (crate-source "windows-version" "0.1.5"
                "1jyah9c8kxqd6bx0hnxkmf859167brbzfw5dg3bs3d121bmn3q39"))

(define rust-windows-x86-64-gnu-0.42.2
  (crate-source "windows_x86_64_gnu" "0.42.2"
                "0dnbf2xnp3xrvy8v9mgs3var4zq9v9yh9kv79035rdgyp2w15scd"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.42.2
  (crate-source "windows_x86_64_gnullvm" "0.42.2"
                "18wl9r8qbsl475j39zvawlidp1bsbinliwfymr43fibdld31pm16"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.42.2
  (crate-source "windows_x86_64_msvc" "0.42.2"
                "1w5r0q0yzx827d10dpjza2ww0j8iajqhmb54s735hhaj66imvv4s"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-winit-0.30.12
  (crate-source "winit" "0.30.12"
                "0cn7wvli4s0l3v5rf6s3rn4j72mdc5p2sxhz6bv0jh4wssg4nvf6"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winnow-0.7.14
  (crate-source "winnow" "0.7.14"
                "0a88ahjqhyn2ln1yplq2xsigm09kxqkdkkk2c2mfxkbzszln8lss"))

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

(define rust-yansi-term-0.1.2
  (crate-source "yansi-term" "0.1.2"
                "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))

(define rust-zbus-5.11.0
  (crate-source "zbus" "5.11.0"
                "1xxdxb95h4cyn8w03yrgrxrpy2pr9x7blqyfn9sy7f2z0dny81rd"))

(define rust-zbus-5.13.2
  (crate-source "zbus" "5.13.2"
                "1ldxqkwy577n7w5ss3lshg9adpyji3vvllj61jr3xahagaczzzhv"))

(define rust-zbus-macros-5.11.0
  (crate-source "zbus_macros" "5.11.0"
                "1jpsvssaxh6fxpbb64dmfnrr8r05rsyfhm32bg63rva7r2lrgrsp"))

(define rust-zbus-macros-5.13.2
  (crate-source "zbus_macros" "5.13.2"
                "1wa6z2gzpzna0mww9jj9db9cq573g914ix6y2ddyxzp8vf85mg8b"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zbus-names-4.3.1
  (crate-source "zbus_names" "4.3.1"
                "03y5f8xwzmk4y5wb4g95a1hl48mxlmhcbwqz62mrnqbqbdnszn7z"))

(define rust-zerocopy-0.8.38
  (crate-source "zerocopy" "0.8.38"
                "0qd9jka18x3p1ifn8nxsx9gw8ldavybzqbc5x4g728svhnk3mksp"))

(define rust-zerocopy-0.8.39
  (crate-source "zerocopy" "0.8.39"
                "0jmf1iqns5sq07k3dscsgyc706pycar67rrq4j9nrnzacgb3avfv"))

(define rust-zerocopy-derive-0.8.38
  (crate-source "zerocopy-derive" "0.8.38"
                "0xfbq46cryk6w4lcwikyg27r8mzgfynrcrgyqhvpha0smy86jqca"))

(define rust-zerocopy-derive-0.8.39
  (crate-source "zerocopy-derive" "0.8.39"
                "05z5yfq0mx3xdqadrgq5sd4d03nl82d9r0vp1qchaip9d4qws8j1"))

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

(define rust-zvariant-5.7.0
  (crate-source "zvariant" "5.7.0"
                "1nwqji0y214dnchq372rsfqzl86d9wgai909s761yay5ffzd77cr"))

(define rust-zvariant-5.9.2
  (crate-source "zvariant" "5.9.2"
                "1i1jn8lvsj79lnfyw21lrsimg2jj0gfj6w6wglrm2y8cyks4xdk8"))

(define rust-zvariant-derive-5.7.0
  (crate-source "zvariant_derive" "5.7.0"
                "13mnhlw8imn6garkqrq4gyxyj7rjnp0hfgqdv5mj4vd44q5zshv6"))

(define rust-zvariant-derive-5.9.2
  (crate-source "zvariant_derive" "5.9.2"
                "0p21bv2kzphhcc71597ya3b0m8hr6wyw2adrqqnbbbxpbsbmska8"))

(define rust-zvariant-utils-3.2.1
  (crate-source "zvariant_utils" "3.2.1"
                "16g5id3h9q85c5vcwdfwkwmwzyladbr2q8x2xinr3xl95wa9v566"))

(define rust-zvariant-utils-3.3.0
  (crate-source "zvariant_utils" "3.3.0"
                "1sf5i71in36gc08jhak83pprnkam8gk936cqlq9hzx7q9sk26p7p"))


(define-cargo-inputs lookup-cargo-inputs
                     (bzmenu =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.1
                                   rust-aho-corasick-1.1.3
                                   rust-anstream-0.6.20
                                   rust-anstyle-1.0.11
                                   rust-anstyle-parse-0.2.7
                                   rust-anstyle-query-1.1.4
                                   rust-anstyle-wincon-3.0.10
                                   rust-anyhow-1.0.100
                                   rust-arc-swap-1.7.1
                                   rust-async-broadcast-0.7.2
                                   rust-async-channel-2.5.0
                                   rust-async-executor-1.13.3
                                   rust-async-io-2.6.0
                                   rust-async-lock-3.4.1
                                   rust-async-process-2.5.0
                                   rust-async-recursion-1.1.1
                                   rust-async-signal-0.2.13
                                   rust-async-task-4.7.1
                                   rust-async-trait-0.1.89
                                   rust-atomic-waker-1.1.2
                                   rust-atty-0.2.14
                                   rust-autocfg-1.5.0
                                   rust-backtrace-0.3.75
                                   rust-base62-2.2.3
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.4
                                   rust-block2-0.6.1
                                   rust-blocking-1.6.2
                                   rust-bluer-0.17.4
                                   rust-bstr-1.12.0
                                   rust-bumpalo-3.19.0
                                   rust-bytes-1.10.1
                                   rust-cc-1.2.38
                                   rust-cfg-if-1.0.3
                                   rust-cfg-aliases-0.2.1
                                   rust-clap-3.2.25
                                   rust-clap-derive-3.2.25
                                   rust-clap-lex-0.2.4
                                   rust-colorchoice-1.0.4
                                   rust-concurrent-queue-2.5.0
                                   rust-crossbeam-deque-0.8.6
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-custom-debug-0.6.2
                                   rust-custom-debug-derive-0.6.2
                                   rust-darling-0.20.11
                                   rust-darling-core-0.20.11
                                   rust-darling-macro-0.20.11
                                   rust-dbus-0.9.9
                                   rust-dbus-crossroads-0.5.2
                                   rust-dbus-tokio-0.7.6
                                   rust-deranged-0.5.3
                                   rust-dispatch2-0.3.0
                                   rust-displaydoc-0.2.5
                                   rust-either-1.15.0
                                   rust-endi-1.1.0
                                   rust-enumflags2-0.7.12
                                   rust-enumflags2-derive-0.7.12
                                   rust-env-filter-0.1.3
                                   rust-env-logger-0.11.8
                                   rust-equivalent-1.0.2
                                   rust-errno-0.3.14
                                   rust-event-listener-5.4.1
                                   rust-event-listener-strategy-0.5.4
                                   rust-fastrand-2.3.0
                                   rust-find-msvc-tools-0.1.2
                                   rust-fnv-1.0.7
                                   rust-futures-0.3.31
                                   rust-futures-channel-0.3.31
                                   rust-futures-core-0.3.31
                                   rust-futures-executor-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-lite-2.6.1
                                   rust-futures-macro-0.3.31
                                   rust-futures-sink-0.3.31
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-getrandom-0.3.3
                                   rust-gimli-0.31.1
                                   rust-glob-0.3.3
                                   rust-globset-0.4.16
                                   rust-globwalk-0.8.1
                                   rust-hashbrown-0.12.3
                                   rust-hashbrown-0.16.0
                                   rust-heck-0.4.1
                                   rust-heck-0.5.0
                                   rust-hermit-abi-0.1.19
                                   rust-hermit-abi-0.5.2
                                   rust-hex-0.4.3
                                   rust-ident-case-1.0.1
                                   rust-ignore-0.4.23
                                   rust-indexmap-1.9.3
                                   rust-indexmap-2.11.4
                                   rust-io-uring-0.7.10
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itertools-0.11.0
                                   rust-itoa-1.0.15
                                   rust-jiff-0.2.15
                                   rust-jiff-static-0.2.15
                                   rust-js-sys-0.3.80
                                   rust-lazy-static-1.5.0
                                   rust-libc-0.2.175
                                   rust-libdbus-sys-0.2.6
                                   rust-linux-raw-sys-0.11.0
                                   rust-log-0.4.28
                                   rust-mac-notification-sys-0.6.6
                                   rust-macaddr-1.0.1
                                   rust-memchr-2.7.5
                                   rust-memoffset-0.9.1
                                   rust-miniz-oxide-0.8.9
                                   rust-mio-1.0.4
                                   rust-nix-0.29.0
                                   rust-nix-0.30.1
                                   rust-normpath-1.5.0
                                   rust-notify-rust-4.11.7
                                   rust-num-conv-0.1.0
                                   rust-num-derive-0.4.2
                                   rust-num-traits-0.2.19
                                   rust-objc2-0.6.2
                                   rust-objc2-core-foundation-0.3.1
                                   rust-objc2-encode-4.1.0
                                   rust-objc2-foundation-0.3.1
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-once-cell-polyfill-1.70.1
                                   rust-ordered-stream-0.2.0
                                   rust-os-str-bytes-6.6.1
                                   rust-parking-2.2.1
                                   rust-pin-project-1.1.10
                                   rust-pin-project-internal-1.1.10
                                   rust-pin-project-lite-0.2.16
                                   rust-pin-utils-0.1.0
                                   rust-piper-0.2.4
                                   rust-pkg-config-0.3.32
                                   rust-polling-3.11.0
                                   rust-portable-atomic-1.11.1
                                   rust-portable-atomic-util-0.2.4
                                   rust-powerfmt-0.2.0
                                   rust-proc-macro-crate-3.4.0
                                   rust-proc-macro-error-1.0.4
                                   rust-proc-macro-error-attr-1.0.4
                                   rust-proc-macro2-1.0.101
                                   rust-process-wrap-8.2.1
                                   rust-quick-xml-0.37.5
                                   rust-quote-1.0.40
                                   rust-r-efi-5.3.0
                                   rust-regex-1.11.2
                                   rust-regex-automata-0.4.10
                                   rust-regex-syntax-0.8.6
                                   rust-rust-i18n-3.1.5
                                   rust-rust-i18n-macro-3.1.5
                                   rust-rust-i18n-support-3.1.5
                                   rust-rustc-demangle-0.1.26
                                   rust-rustix-1.1.2
                                   rust-rustversion-1.0.22
                                   rust-ryu-1.0.20
                                   rust-same-file-1.0.6
                                   rust-serde-1.0.226
                                   rust-serde-core-1.0.226
                                   rust-serde-derive-1.0.226
                                   rust-serde-json-1.0.145
                                   rust-serde-repr-0.1.20
                                   rust-serde-spanned-0.6.9
                                   rust-serde-yaml-0.9.34+deprecated
                                   rust-shlex-1.3.0
                                   rust-signal-hook-0.3.18
                                   rust-signal-hook-registry-1.4.6
                                   rust-siphasher-1.0.1
                                   rust-slab-0.4.11
                                   rust-smallvec-1.15.1
                                   rust-socket2-0.6.0
                                   rust-stable-deref-trait-1.2.0
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.10.0
                                   rust-strsim-0.11.1
                                   rust-strum-0.26.3
                                   rust-strum-macros-0.26.4
                                   rust-syn-1.0.109
                                   rust-syn-2.0.106
                                   rust-synstructure-0.13.2
                                   rust-sys-locale-0.3.2
                                   rust-tauri-winrt-notification-0.7.2
                                   rust-tempfile-3.22.0
                                   rust-termcolor-1.4.1
                                   rust-textwrap-0.16.2
                                   rust-thiserror-2.0.16
                                   rust-thiserror-impl-2.0.16
                                   rust-time-0.3.44
                                   rust-time-core-0.1.6
                                   rust-tokio-1.47.1
                                   rust-tokio-macros-2.5.0
                                   rust-tokio-stream-0.1.17
                                   rust-toml-0.8.23
                                   rust-toml-datetime-0.6.11
                                   rust-toml-datetime-0.7.2
                                   rust-toml-edit-0.22.27
                                   rust-toml-edit-0.23.6
                                   rust-toml-parser-1.0.3
                                   rust-toml-write-0.1.2
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.30
                                   rust-tracing-core-0.1.34
                                   rust-triomphe-0.1.14
                                   rust-uds-windows-1.1.0
                                   rust-unicode-ident-1.0.19
                                   rust-unsafe-libyaml-0.2.11
                                   rust-utf8parse-0.2.2
                                   rust-uuid-1.18.1
                                   rust-version-check-0.9.5
                                   rust-walkdir-2.5.0
                                   rust-wasi-0.11.1+wasi-snapshot-preview1
                                   rust-wasi-0.14.7+wasi-0.2.4
                                   rust-wasip2-1.0.1+wasi-0.2.4
                                   rust-wasm-bindgen-0.2.103
                                   rust-wasm-bindgen-backend-0.2.103
                                   rust-wasm-bindgen-macro-0.2.103
                                   rust-wasm-bindgen-macro-support-0.2.103
                                   rust-wasm-bindgen-shared-0.2.103
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.11
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-0.61.3
                                   rust-windows-collections-0.2.0
                                   rust-windows-core-0.61.2
                                   rust-windows-future-0.2.1
                                   rust-windows-implement-0.60.0
                                   rust-windows-interface-0.59.1
                                   rust-windows-link-0.1.3
                                   rust-windows-link-0.2.0
                                   rust-windows-numerics-0.2.0
                                   rust-windows-result-0.3.4
                                   rust-windows-strings-0.4.2
                                   rust-windows-sys-0.59.0
                                   rust-windows-sys-0.60.2
                                   rust-windows-sys-0.61.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-targets-0.53.3
                                   rust-windows-threading-0.1.0
                                   rust-windows-version-0.1.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-gnullvm-0.53.0
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-aarch64-msvc-0.53.0
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnu-0.53.0
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-gnullvm-0.53.0
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-i686-msvc-0.53.0
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnu-0.53.0
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-gnullvm-0.53.0
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-windows-x86-64-msvc-0.53.0
                                   rust-winnow-0.7.13
                                   rust-wit-bindgen-0.46.0
                                   rust-zbus-5.11.0
                                   rust-zbus-macros-5.11.0
                                   rust-zbus-names-4.2.0
                                   rust-zvariant-5.7.0
                                   rust-zvariant-derive-5.7.0
                                   rust-zvariant-utils-3.2.1))
                     (ewm =>
                          (list rust-aho-corasick-1.1.4
                                rust-aliasable-0.1.3
                                rust-annotate-snippets-0.11.5
                                rust-anstyle-1.0.13
                                rust-anyhow-1.0.101
                                rust-appendlist-1.4.0
                                rust-approx-0.4.0
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
                                rust-atomic-float-1.1.0
                                rust-autocfg-1.5.0
                                rust-bindgen-0.72.1
                                rust-bit-set-0.8.0
                                rust-bit-vec-0.8.0
                                rust-bitflags-2.10.0
                                rust-block-buffer-0.10.4
                                rust-blocking-1.6.2
                                rust-bumpalo-3.19.1
                                rust-bytemuck-1.25.0
                                rust-bytemuck-derive-1.10.2
                                rust-calloop-0.14.3
                                rust-cc-1.2.55
                                rust-cexpr-0.6.0
                                rust-cfg-expr-0.20.6
                                rust-cfg-if-1.0.4
                                rust-cfg-aliases-0.2.1
                                rust-cgmath-0.18.0
                                rust-clang-sys-1.8.1
                                rust-concurrent-queue-2.5.0
                                rust-console-0.15.11
                                rust-convert-case-0.8.0
                                rust-cookie-factory-0.3.3
                                rust-cpufeatures-0.2.17
                                rust-crossbeam-utils-0.8.21
                                rust-crypto-common-0.1.7
                                rust-ctor-0.1.26
                                rust-cursor-icon-1.2.0
                                rust-darling-0.10.2
                                rust-darling-core-0.10.2
                                rust-darling-macro-0.10.2
                                rust-digest-0.10.7
                                rust-dlib-0.5.2
                                rust-downcast-rs-1.2.1
                                rust-drm-0.14.1
                                rust-drm-ffi-0.9.0
                                rust-drm-fourcc-2.2.0
                                rust-drm-sys-0.8.0
                                rust-either-1.15.0
                                rust-emacs-0.19.0
                                rust-emacs-macros-0.17.0
                                rust-emacs-module-0.18.0
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
                                rust-generic-array-0.14.7
                                rust-getrandom-0.3.4
                                rust-gio-0.20.12
                                rust-gio-sys-0.20.10
                                rust-gl-generator-0.14.0
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
                                rust-input-0.9.1
                                rust-input-sys-1.18.0
                                rust-insta-1.46.3
                                rust-io-lifetimes-1.0.11
                                rust-itertools-0.12.1
                                rust-itoa-1.0.17
                                rust-js-sys-0.3.85
                                rust-khronos-api-3.1.0
                                rust-lazy-static-1.5.0
                                rust-libc-0.2.180
                                rust-libloading-0.8.9
                                rust-libseat-0.2.4
                                rust-libseat-sys-0.2.0
                                rust-libspa-0.9.2
                                rust-libspa-sys-0.9.2
                                rust-libudev-sys-0.1.4
                                rust-linux-raw-sys-0.4.15
                                rust-linux-raw-sys-0.6.5
                                rust-linux-raw-sys-0.11.0
                                rust-log-0.4.29
                                rust-loom-0.7.2
                                rust-matchers-0.1.0
                                rust-memchr-2.7.6
                                rust-memmap2-0.9.9
                                rust-memoffset-0.9.1
                                rust-minimal-lexical-0.2.1
                                rust-nix-0.30.1
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
                                rust-pipewire-0.9.2
                                rust-pipewire-sys-0.9.2
                                rust-pkg-config-0.3.32
                                rust-polling-3.11.0
                                rust-ppv-lite86-0.2.21
                                rust-proc-macro-crate-3.4.0
                                rust-proc-macro2-1.0.106
                                rust-profiling-1.0.17
                                rust-profiling-procmacros-1.0.17
                                rust-proptest-1.10.0
                                rust-quick-error-1.2.3
                                rust-quick-xml-0.38.4
                                rust-quote-1.0.44
                                rust-r-efi-5.3.0
                                rust-rand-0.9.2
                                rust-rand-chacha-0.9.0
                                rust-rand-core-0.9.5
                                rust-rand-xorshift-0.4.0
                                rust-regex-1.12.3
                                rust-regex-automata-0.1.10
                                rust-regex-automata-0.4.14
                                rust-regex-syntax-0.6.29
                                rust-regex-syntax-0.8.9
                                rust-rustc-hash-2.1.1
                                rust-rustc-version-0.2.3
                                rust-rustix-0.38.44
                                rust-rustix-1.1.3
                                rust-rustversion-1.0.22
                                rust-rusty-fork-0.3.1
                                rust-scoped-tls-1.0.1
                                rust-sd-notify-0.4.5
                                rust-semver-0.9.0
                                rust-semver-parser-0.7.0
                                rust-serde-1.0.228
                                rust-serde-core-1.0.228
                                rust-serde-derive-1.0.228
                                rust-serde-json-1.0.149
                                rust-serde-repr-0.1.20
                                rust-serde-spanned-1.0.4
                                rust-sha2-0.10.9
                                rust-sharded-slab-0.1.7
                                rust-shlex-1.3.0
                                rust-signal-hook-registry-1.4.8
                                rust-similar-2.7.0
                                rust-slab-0.4.12
                                rust-smallvec-1.15.1
                                rust-smithay-0.7.0.ae14fa1
                                rust-smithay-drm-extras-0.1.0.ae14fa1
                                rust-strsim-0.9.3
                                rust-syn-1.0.109
                                rust-syn-2.0.114
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
                                rust-typenum-1.19.0
                                rust-udev-0.9.3
                                rust-uds-windows-1.1.0
                                rust-unarray-0.1.4
                                rust-unicode-ident-1.0.22
                                rust-unicode-segmentation-1.12.0
                                rust-unicode-width-0.2.2
                                rust-uuid-1.20.0
                                rust-valuable-0.1.1
                                rust-version-compare-0.2.1
                                rust-version-check-0.9.5
                                rust-wait-timeout-0.2.1
                                rust-wasip2-1.0.2+wasi-0.2.9
                                rust-wasm-bindgen-0.2.108
                                rust-wasm-bindgen-macro-0.2.108
                                rust-wasm-bindgen-macro-support-0.2.108
                                rust-wasm-bindgen-shared-0.2.108
                                rust-wayland-backend-0.3.12
                                rust-wayland-client-0.31.12
                                rust-wayland-protocols-0.32.10
                                rust-wayland-protocols-misc-0.3.10
                                rust-wayland-protocols-wlr-0.3.10
                                rust-wayland-scanner-0.31.8
                                rust-wayland-server-0.31.11
                                rust-wayland-sys-0.31.8
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
                                rust-wit-bindgen-0.51.0
                                rust-xkbcommon-0.9.0
                                rust-xkeysym-0.2.1
                                rust-xml-rs-0.8.28
                                rust-zbus-5.13.2
                                rust-zbus-macros-5.13.2
                                rust-zbus-names-4.3.1
                                rust-zerocopy-0.8.38
                                rust-zerocopy-derive-0.8.38
                                rust-zmij-1.0.19
                                rust-zvariant-5.9.2
                                rust-zvariant-derive-5.9.2
                                rust-zvariant-utils-3.3.0))
                     (iwmenu =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.1
                                   rust-aho-corasick-1.1.3
                                   rust-anstream-0.6.20
                                   rust-anstyle-1.0.11
                                   rust-anstyle-parse-0.2.7
                                   rust-anstyle-query-1.1.4
                                   rust-anstyle-wincon-3.0.10
                                   rust-anyhow-1.0.100
                                   rust-arc-swap-1.7.1
                                   rust-async-broadcast-0.7.2
                                   rust-async-channel-2.5.0
                                   rust-async-executor-1.13.3
                                   rust-async-io-2.6.0
                                   rust-async-lock-3.4.1
                                   rust-async-process-2.5.0
                                   rust-async-recursion-1.1.1
                                   rust-async-signal-0.2.13
                                   rust-async-task-4.7.1
                                   rust-async-trait-0.1.89
                                   rust-atomic-waker-1.1.2
                                   rust-atty-0.2.14
                                   rust-autocfg-1.5.0
                                   rust-backtrace-0.3.75
                                   rust-base62-2.2.3
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.4
                                   rust-block2-0.6.1
                                   rust-blocking-1.6.2
                                   rust-bstr-1.12.0
                                   rust-bumpalo-3.19.0
                                   rust-cc-1.2.38
                                   rust-cfg-if-1.0.3
                                   rust-cfg-aliases-0.2.1
                                   rust-clap-3.2.25
                                   rust-clap-derive-3.2.25
                                   rust-clap-lex-0.2.4
                                   rust-colorchoice-1.0.4
                                   rust-concurrent-queue-2.5.0
                                   rust-crossbeam-deque-0.8.6
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-deranged-0.5.3
                                   rust-dispatch2-0.3.0
                                   rust-either-1.15.0
                                   rust-endi-1.1.0
                                   rust-enumflags2-0.7.12
                                   rust-enumflags2-derive-0.7.12
                                   rust-env-filter-0.1.3
                                   rust-env-logger-0.11.8
                                   rust-equivalent-1.0.2
                                   rust-errno-0.3.14
                                   rust-event-listener-5.4.1
                                   rust-event-listener-strategy-0.5.4
                                   rust-fastrand-2.3.0
                                   rust-find-msvc-tools-0.1.2
                                   rust-futures-core-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-lite-2.6.1
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-getrandom-0.3.3
                                   rust-gimli-0.31.1
                                   rust-glob-0.3.3
                                   rust-globset-0.4.16
                                   rust-globwalk-0.8.1
                                   rust-hashbrown-0.12.3
                                   rust-hashbrown-0.16.0
                                   rust-heck-0.4.1
                                   rust-hermit-abi-0.1.19
                                   rust-hermit-abi-0.5.2
                                   rust-hex-0.4.3
                                   rust-ignore-0.4.23
                                   rust-indexmap-1.9.3
                                   rust-indexmap-2.11.4
                                   rust-io-uring-0.7.10
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itertools-0.11.0
                                   rust-itoa-1.0.15
                                   rust-iwdrs-0.1.6
                                   rust-jiff-0.2.15
                                   rust-jiff-static-0.2.15
                                   rust-js-sys-0.3.80
                                   rust-lazy-static-1.5.0
                                   rust-libc-0.2.175
                                   rust-linux-raw-sys-0.11.0
                                   rust-log-0.4.28
                                   rust-mac-notification-sys-0.6.6
                                   rust-memchr-2.7.5
                                   rust-memoffset-0.9.1
                                   rust-miniz-oxide-0.8.9
                                   rust-mio-1.0.4
                                   rust-nix-0.29.0
                                   rust-nix-0.30.1
                                   rust-normpath-1.5.0
                                   rust-notify-rust-4.11.7
                                   rust-num-conv-0.1.0
                                   rust-objc2-0.6.2
                                   rust-objc2-core-foundation-0.3.1
                                   rust-objc2-encode-4.1.0
                                   rust-objc2-foundation-0.3.1
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-once-cell-polyfill-1.70.1
                                   rust-ordered-stream-0.2.0
                                   rust-os-str-bytes-6.6.1
                                   rust-parking-2.2.1
                                   rust-pin-project-lite-0.2.16
                                   rust-pin-utils-0.1.0
                                   rust-piper-0.2.4
                                   rust-polling-3.11.0
                                   rust-portable-atomic-1.11.1
                                   rust-portable-atomic-util-0.2.4
                                   rust-powerfmt-0.2.0
                                   rust-proc-macro-crate-3.4.0
                                   rust-proc-macro-error-1.0.4
                                   rust-proc-macro-error-attr-1.0.4
                                   rust-proc-macro2-1.0.101
                                   rust-process-wrap-8.2.1
                                   rust-quick-xml-0.37.5
                                   rust-quote-1.0.40
                                   rust-r-efi-5.3.0
                                   rust-regex-1.11.2
                                   rust-regex-automata-0.4.10
                                   rust-regex-syntax-0.8.6
                                   rust-rust-i18n-3.1.5
                                   rust-rust-i18n-macro-3.1.5
                                   rust-rust-i18n-support-3.1.5
                                   rust-rustc-demangle-0.1.26
                                   rust-rustix-1.1.2
                                   rust-rustversion-1.0.22
                                   rust-ryu-1.0.20
                                   rust-same-file-1.0.6
                                   rust-serde-1.0.226
                                   rust-serde-core-1.0.226
                                   rust-serde-derive-1.0.226
                                   rust-serde-json-1.0.145
                                   rust-serde-repr-0.1.20
                                   rust-serde-spanned-0.6.9
                                   rust-serde-yaml-0.9.34+deprecated
                                   rust-shlex-1.3.0
                                   rust-signal-hook-0.3.18
                                   rust-signal-hook-registry-1.4.6
                                   rust-siphasher-1.0.1
                                   rust-slab-0.4.11
                                   rust-smallvec-1.15.1
                                   rust-stable-deref-trait-1.2.0
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.10.0
                                   rust-syn-1.0.109
                                   rust-syn-2.0.106
                                   rust-sys-locale-0.3.2
                                   rust-tauri-winrt-notification-0.7.2
                                   rust-tempfile-3.22.0
                                   rust-termcolor-1.4.1
                                   rust-textwrap-0.16.2
                                   rust-thiserror-2.0.16
                                   rust-thiserror-impl-2.0.16
                                   rust-time-0.3.44
                                   rust-time-core-0.1.6
                                   rust-tokio-1.47.1
                                   rust-tokio-macros-2.5.0
                                   rust-toml-0.8.23
                                   rust-toml-datetime-0.6.11
                                   rust-toml-datetime-0.7.2
                                   rust-toml-edit-0.22.27
                                   rust-toml-edit-0.23.6
                                   rust-toml-parser-1.0.3
                                   rust-toml-write-0.1.2
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.30
                                   rust-tracing-core-0.1.34
                                   rust-triomphe-0.1.14
                                   rust-uds-windows-1.1.0
                                   rust-unicode-ident-1.0.19
                                   rust-unsafe-libyaml-0.2.11
                                   rust-utf8parse-0.2.2
                                   rust-uuid-1.18.1
                                   rust-version-check-0.9.5
                                   rust-walkdir-2.5.0
                                   rust-wasi-0.11.1+wasi-snapshot-preview1
                                   rust-wasi-0.14.7+wasi-0.2.4
                                   rust-wasip2-1.0.1+wasi-0.2.4
                                   rust-wasm-bindgen-0.2.103
                                   rust-wasm-bindgen-backend-0.2.103
                                   rust-wasm-bindgen-macro-0.2.103
                                   rust-wasm-bindgen-macro-support-0.2.103
                                   rust-wasm-bindgen-shared-0.2.103
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.11
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-0.61.3
                                   rust-windows-collections-0.2.0
                                   rust-windows-core-0.61.2
                                   rust-windows-future-0.2.1
                                   rust-windows-implement-0.60.0
                                   rust-windows-interface-0.59.1
                                   rust-windows-link-0.1.3
                                   rust-windows-link-0.2.0
                                   rust-windows-numerics-0.2.0
                                   rust-windows-result-0.3.4
                                   rust-windows-strings-0.4.2
                                   rust-windows-sys-0.59.0
                                   rust-windows-sys-0.60.2
                                   rust-windows-sys-0.61.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-targets-0.53.3
                                   rust-windows-threading-0.1.0
                                   rust-windows-version-0.1.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-gnullvm-0.53.0
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-aarch64-msvc-0.53.0
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnu-0.53.0
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-gnullvm-0.53.0
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-i686-msvc-0.53.0
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnu-0.53.0
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-gnullvm-0.53.0
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-windows-x86-64-msvc-0.53.0
                                   rust-winnow-0.7.13
                                   rust-wit-bindgen-0.46.0
                                   rust-zbus-5.11.0
                                   rust-zbus-macros-5.11.0
                                   rust-zbus-names-4.2.0
                                   rust-zvariant-5.7.0
                                   rust-zvariant-derive-5.7.0
                                   rust-zvariant-utils-3.2.1))
                     (pwmenu =>
                             (list rust-addr2line-0.24.2
                                   rust-adler2-2.0.1
                                   rust-aho-corasick-1.1.3
                                   rust-annotate-snippets-0.9.2
                                   rust-anstream-0.6.20
                                   rust-anstyle-1.0.11
                                   rust-anstyle-parse-0.2.7
                                   rust-anstyle-query-1.1.4
                                   rust-anstyle-wincon-3.0.10
                                   rust-anyhow-1.0.100
                                   rust-arc-swap-1.7.1
                                   rust-async-broadcast-0.7.2
                                   rust-async-channel-2.5.0
                                   rust-async-executor-1.13.3
                                   rust-async-io-2.6.0
                                   rust-async-lock-3.4.1
                                   rust-async-process-2.5.0
                                   rust-async-recursion-1.1.1
                                   rust-async-signal-0.2.13
                                   rust-async-task-4.7.1
                                   rust-async-trait-0.1.89
                                   rust-atomic-waker-1.1.2
                                   rust-atty-0.2.14
                                   rust-autocfg-1.5.0
                                   rust-backtrace-0.3.75
                                   rust-base62-2.2.3
                                   rust-bindgen-0.69.5
                                   rust-bitflags-1.3.2
                                   rust-bitflags-2.9.4
                                   rust-block2-0.6.1
                                   rust-blocking-1.6.2
                                   rust-bstr-1.12.0
                                   rust-cc-1.2.38
                                   rust-cexpr-0.6.0
                                   rust-cfg-expr-0.15.8
                                   rust-cfg-if-1.0.3
                                   rust-cfg-aliases-0.2.1
                                   rust-clang-sys-1.8.1
                                   rust-clap-3.2.25
                                   rust-clap-derive-3.2.25
                                   rust-clap-lex-0.2.4
                                   rust-colorchoice-1.0.4
                                   rust-concurrent-queue-2.5.0
                                   rust-convert-case-0.6.0
                                   rust-cookie-factory-0.3.3
                                   rust-crossbeam-deque-0.8.6
                                   rust-crossbeam-epoch-0.9.18
                                   rust-crossbeam-utils-0.8.21
                                   rust-deranged-0.5.3
                                   rust-dispatch2-0.3.0
                                   rust-either-1.15.0
                                   rust-endi-1.1.0
                                   rust-enumflags2-0.7.12
                                   rust-enumflags2-derive-0.7.12
                                   rust-env-filter-0.1.3
                                   rust-env-logger-0.11.8
                                   rust-equivalent-1.0.2
                                   rust-errno-0.3.14
                                   rust-event-listener-5.4.1
                                   rust-event-listener-strategy-0.5.4
                                   rust-fastrand-2.3.0
                                   rust-find-msvc-tools-0.1.2
                                   rust-futures-0.3.31
                                   rust-futures-channel-0.3.31
                                   rust-futures-core-0.3.31
                                   rust-futures-executor-0.3.31
                                   rust-futures-io-0.3.31
                                   rust-futures-lite-2.6.1
                                   rust-futures-macro-0.3.31
                                   rust-futures-sink-0.3.31
                                   rust-futures-task-0.3.31
                                   rust-futures-util-0.3.31
                                   rust-getrandom-0.3.3
                                   rust-gimli-0.31.1
                                   rust-glob-0.3.3
                                   rust-globset-0.4.16
                                   rust-globwalk-0.8.1
                                   rust-hashbrown-0.12.3
                                   rust-hashbrown-0.16.0
                                   rust-heck-0.4.1
                                   rust-heck-0.5.0
                                   rust-hermit-abi-0.1.19
                                   rust-hermit-abi-0.5.2
                                   rust-hex-0.4.3
                                   rust-ignore-0.4.23
                                   rust-indexmap-1.9.3
                                   rust-indexmap-2.11.4
                                   rust-io-uring-0.7.10
                                   rust-is-terminal-polyfill-1.70.1
                                   rust-itertools-0.11.0
                                   rust-itertools-0.12.1
                                   rust-itoa-1.0.15
                                   rust-jiff-0.2.15
                                   rust-jiff-static-0.2.15
                                   rust-lazy-static-1.5.0
                                   rust-lazycell-1.3.0
                                   rust-libc-0.2.175
                                   rust-libloading-0.8.9
                                   rust-libspa-0.8.0
                                   rust-libspa-sys-0.8.0
                                   rust-linux-raw-sys-0.11.0
                                   rust-log-0.4.28
                                   rust-mac-notification-sys-0.6.6
                                   rust-memchr-2.7.5
                                   rust-memoffset-0.9.1
                                   rust-minimal-lexical-0.2.1
                                   rust-miniz-oxide-0.8.9
                                   rust-mio-1.0.4
                                   rust-nix-0.27.1
                                   rust-nix-0.29.0
                                   rust-nix-0.30.1
                                   rust-nom-7.1.3
                                   rust-normpath-1.5.0
                                   rust-notify-rust-4.11.7
                                   rust-num-conv-0.1.0
                                   rust-objc2-0.6.2
                                   rust-objc2-core-foundation-0.3.1
                                   rust-objc2-encode-4.1.0
                                   rust-objc2-foundation-0.3.1
                                   rust-object-0.36.7
                                   rust-once-cell-1.21.3
                                   rust-once-cell-polyfill-1.70.1
                                   rust-ordered-stream-0.2.0
                                   rust-os-str-bytes-6.6.1
                                   rust-parking-2.2.1
                                   rust-pin-project-lite-0.2.16
                                   rust-pin-utils-0.1.0
                                   rust-piper-0.2.4
                                   rust-pipewire-0.8.0
                                   rust-pipewire-sys-0.8.0
                                   rust-pkg-config-0.3.32
                                   rust-polling-3.11.0
                                   rust-portable-atomic-1.11.1
                                   rust-portable-atomic-util-0.2.4
                                   rust-powerfmt-0.2.0
                                   rust-proc-macro-crate-3.4.0
                                   rust-proc-macro-error-1.0.4
                                   rust-proc-macro-error-attr-1.0.4
                                   rust-proc-macro2-1.0.101
                                   rust-process-wrap-8.2.1
                                   rust-quick-xml-0.37.5
                                   rust-quote-1.0.40
                                   rust-r-efi-5.3.0
                                   rust-regex-1.11.2
                                   rust-regex-automata-0.4.10
                                   rust-regex-syntax-0.8.6
                                   rust-rust-i18n-3.1.5
                                   rust-rust-i18n-macro-3.1.5
                                   rust-rust-i18n-support-3.1.5
                                   rust-rustc-demangle-0.1.26
                                   rust-rustc-hash-1.1.0
                                   rust-rustix-1.1.2
                                   rust-ryu-1.0.20
                                   rust-same-file-1.0.6
                                   rust-serde-1.0.226
                                   rust-serde-core-1.0.226
                                   rust-serde-derive-1.0.226
                                   rust-serde-json-1.0.145
                                   rust-serde-repr-0.1.20
                                   rust-serde-spanned-0.6.9
                                   rust-serde-yaml-0.9.34+deprecated
                                   rust-shlex-1.3.0
                                   rust-signal-hook-0.3.18
                                   rust-signal-hook-registry-1.4.6
                                   rust-siphasher-1.0.1
                                   rust-slab-0.4.11
                                   rust-smallvec-1.15.1
                                   rust-stable-deref-trait-1.2.0
                                   rust-static-assertions-1.1.0
                                   rust-strsim-0.10.0
                                   rust-syn-1.0.109
                                   rust-syn-2.0.106
                                   rust-sys-locale-0.3.2
                                   rust-system-deps-6.2.2
                                   rust-target-lexicon-0.12.16
                                   rust-tauri-winrt-notification-0.7.2
                                   rust-tempfile-3.22.0
                                   rust-termcolor-1.4.1
                                   rust-textwrap-0.16.2
                                   rust-thiserror-1.0.69
                                   rust-thiserror-2.0.16
                                   rust-thiserror-impl-1.0.69
                                   rust-thiserror-impl-2.0.16
                                   rust-time-0.3.44
                                   rust-time-core-0.1.6
                                   rust-tokio-1.47.1
                                   rust-tokio-macros-2.5.0
                                   rust-toml-0.8.23
                                   rust-toml-datetime-0.6.11
                                   rust-toml-datetime-0.7.2
                                   rust-toml-edit-0.22.27
                                   rust-toml-edit-0.23.6
                                   rust-toml-parser-1.0.3
                                   rust-toml-write-0.1.2
                                   rust-tracing-0.1.41
                                   rust-tracing-attributes-0.1.30
                                   rust-tracing-core-0.1.34
                                   rust-triomphe-0.1.14
                                   rust-uds-windows-1.1.0
                                   rust-unicode-ident-1.0.19
                                   rust-unicode-segmentation-1.12.0
                                   rust-unicode-width-0.1.14
                                   rust-unsafe-libyaml-0.2.11
                                   rust-utf8parse-0.2.2
                                   rust-version-compare-0.2.0
                                   rust-version-check-0.9.5
                                   rust-walkdir-2.5.0
                                   rust-wasi-0.11.1+wasi-snapshot-preview1
                                   rust-wasi-0.14.7+wasi-0.2.4
                                   rust-wasip2-1.0.1+wasi-0.2.4
                                   rust-winapi-0.3.9
                                   rust-winapi-i686-pc-windows-gnu-0.4.0
                                   rust-winapi-util-0.1.11
                                   rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                   rust-windows-0.61.3
                                   rust-windows-collections-0.2.0
                                   rust-windows-core-0.61.2
                                   rust-windows-future-0.2.1
                                   rust-windows-implement-0.60.0
                                   rust-windows-interface-0.59.1
                                   rust-windows-link-0.1.3
                                   rust-windows-link-0.2.0
                                   rust-windows-numerics-0.2.0
                                   rust-windows-result-0.3.4
                                   rust-windows-strings-0.4.2
                                   rust-windows-sys-0.59.0
                                   rust-windows-sys-0.60.2
                                   rust-windows-sys-0.61.0
                                   rust-windows-targets-0.52.6
                                   rust-windows-targets-0.53.3
                                   rust-windows-threading-0.1.0
                                   rust-windows-version-0.1.5
                                   rust-windows-aarch64-gnullvm-0.52.6
                                   rust-windows-aarch64-gnullvm-0.53.0
                                   rust-windows-aarch64-msvc-0.52.6
                                   rust-windows-aarch64-msvc-0.53.0
                                   rust-windows-i686-gnu-0.52.6
                                   rust-windows-i686-gnu-0.53.0
                                   rust-windows-i686-gnullvm-0.52.6
                                   rust-windows-i686-gnullvm-0.53.0
                                   rust-windows-i686-msvc-0.52.6
                                   rust-windows-i686-msvc-0.53.0
                                   rust-windows-x86-64-gnu-0.52.6
                                   rust-windows-x86-64-gnu-0.53.0
                                   rust-windows-x86-64-gnullvm-0.52.6
                                   rust-windows-x86-64-gnullvm-0.53.0
                                   rust-windows-x86-64-msvc-0.52.6
                                   rust-windows-x86-64-msvc-0.53.0
                                   rust-winnow-0.7.13
                                   rust-wit-bindgen-0.46.0
                                   rust-yansi-term-0.1.2
                                   rust-zbus-5.11.0
                                   rust-zbus-macros-5.11.0
                                   rust-zbus-names-4.2.0
                                   rust-zvariant-5.7.0
                                   rust-zvariant-derive-5.7.0
                                   rust-zvariant-utils-3.2.1))
                     (rust-smithay-0.7.0.ae14fa1 =>
                              (list rust-adler2-2.0.1
                               rust-ahash-0.8.12
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
                               rust-approx-0.4.0
                               rust-arbitrary-1.4.2
                               rust-arg-enum-proc-macro-0.3.4
                               rust-arrayvec-0.7.6
                               rust-as-raw-xcb-connection-1.0.1
                               rust-as-slice-0.2.1
                               rust-ash-0.38.0+1.3.281
                               rust-atomic-waker-1.1.2
                               rust-atomic-float-1.1.0
                               rust-autocfg-1.5.0
                               rust-av-scenechange-0.14.1
                               rust-av1-grain-0.2.5
                               rust-avif-serialize-0.8.8
                               rust-bincode-1.3.3
                               rust-bindgen-0.69.5
                               rust-bit-field-0.10.3
                               rust-bitflags-1.3.2
                               rust-bitflags-2.11.0
                               rust-bitstream-io-4.9.0
                               rust-block-buffer-0.10.4
                               rust-block2-0.5.1
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
                               rust-cast-0.3.0
                               rust-cc-1.2.56
                               rust-cesu8-1.1.0
                               rust-cexpr-0.6.0
                               rust-cfg-expr-0.20.6
                               rust-cfg-if-1.0.4
                               rust-cfg-aliases-0.2.1
                               rust-cgmath-0.18.0
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
                               rust-core-foundation-0.9.4
                               rust-core-foundation-sys-0.8.7
                               rust-core-graphics-0.23.2
                               rust-core-graphics-types-0.1.3
                               rust-core2-0.4.0
                               rust-cpufeatures-0.2.17
                               rust-crc32fast-1.5.0
                               rust-criterion-0.5.1
                               rust-criterion-plot-0.5.0
                               rust-crossbeam-channel-0.5.15
                               rust-crossbeam-deque-0.8.6
                               rust-crossbeam-epoch-0.9.18
                               rust-crossbeam-utils-0.8.21
                               rust-crunchy-0.2.4
                               rust-crypto-common-0.1.7
                               rust-cursor-icon-1.2.0
                               rust-digest-0.10.7
                               rust-dispatch-0.2.0
                               rust-dlib-0.5.2
                               rust-downcast-rs-1.2.1
                               rust-dpi-0.1.2
                               rust-drm-0.14.1
                               rust-drm-ffi-0.9.0
                               rust-drm-fourcc-2.2.0
                               rust-drm-sys-0.8.0
                               rust-either-1.15.0
                               rust-encoding-rs-0.8.35
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
                               rust-foreign-types-0.5.0
                               rust-foreign-types-macros-0.2.3
                               rust-foreign-types-shared-0.3.1
                               rust-fps-ticker-1.0.0
                               rust-futures-core-0.3.32
                               rust-futures-task-0.3.32
                               rust-futures-util-0.3.32
                               rust-gbm-0.18.0
                               rust-gbm-sys-0.4.0
                               rust-generator-0.8.8
                               rust-generic-array-0.14.7
                               rust-gethostname-1.1.0
                               rust-getrandom-0.2.17
                               rust-getrandom-0.3.4
                               rust-getrandom-0.4.1
                               rust-gif-0.14.1
                               rust-gl-generator-0.14.0
                               rust-glob-0.3.3
                               rust-glow-0.16.0
                               rust-half-2.7.1
                               rust-hashbrown-0.15.5
                               rust-hashbrown-0.16.1
                               rust-heck-0.5.0
                               rust-hermit-abi-0.3.9
                               rust-hermit-abi-0.5.2
                               rust-home-0.5.11
                               rust-id-arena-2.3.0
                               rust-image-0.25.9
                               rust-image-webp-0.2.4
                               rust-imgref-1.12.0
                               rust-indexmap-2.13.0
                               rust-input-0.9.1
                               rust-input-sys-1.18.0
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
                               rust-libredox-0.1.12
                               rust-libseat-0.2.4
                               rust-libseat-sys-0.2.0
                               rust-libudev-sys-0.1.4
                               rust-linux-raw-sys-0.4.15
                               rust-linux-raw-sys-0.6.5
                               rust-linux-raw-sys-0.12.1
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
                               rust-objc-sys-0.3.5
                               rust-objc2-0.5.2
                               rust-objc2-app-kit-0.2.2
                               rust-objc2-cloud-kit-0.2.2
                               rust-objc2-contacts-0.2.2
                               rust-objc2-core-data-0.2.2
                               rust-objc2-core-image-0.2.2
                               rust-objc2-core-location-0.2.2
                               rust-objc2-encode-4.1.0
                               rust-objc2-foundation-0.2.2
                               rust-objc2-link-presentation-0.2.2
                               rust-objc2-metal-0.2.2
                               rust-objc2-quartz-core-0.2.2
                               rust-objc2-symbols-0.2.2
                               rust-objc2-ui-kit-0.2.2
                               rust-objc2-uniform-type-identifiers-0.2.2
                               rust-objc2-user-notifications-0.2.2
                               rust-once-cell-1.21.3
                               rust-once-cell-polyfill-1.70.2
                               rust-oorandom-11.1.5
                               rust-orbclient-0.3.50
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
                               rust-plotters-0.3.7
                               rust-plotters-backend-0.3.7
                               rust-plotters-svg-0.3.7
                               rust-png-0.18.1
                               rust-polling-3.11.0
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
                               rust-quick-xml-0.38.4
                               rust-quote-1.0.44
                               rust-r-efi-5.3.0
                               rust-rand-0.8.5
                               rust-rand-0.9.2
                               rust-rand-chacha-0.3.1
                               rust-rand-chacha-0.9.0
                               rust-rand-core-0.6.4
                               rust-rand-core-0.9.5
                               rust-rav1e-0.8.1
                               rust-ravif-0.12.0
                               rust-raw-window-handle-0.6.2
                               rust-rayon-1.11.0
                               rust-rayon-core-1.13.0
                               rust-redox-syscall-0.4.1
                               rust-redox-syscall-0.5.18
                               rust-redox-syscall-0.7.2
                               rust-regex-1.12.3
                               rust-regex-automata-0.4.14
                               rust-regex-syntax-0.8.10
                               rust-renderdoc-0.11.0
                               rust-renderdoc-sys-1.1.0
                               rust-rgb-0.8.52
                               rust-rustc-hash-1.1.0
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
                               rust-sha2-0.10.9
                               rust-sharded-slab-0.1.7
                               rust-shlex-1.3.0
                               rust-simd-adler32-0.3.8
                               rust-simd-helpers-0.1.0
                               rust-slab-0.4.12
                               rust-slotmap-1.1.1
                               rust-smallvec-1.15.1
                               rust-smithay-client-toolkit-0.19.2
                               rust-smol-str-0.2.2
                               rust-stable-deref-trait-1.2.1
                               rust-strsim-0.11.1
                               rust-syn-2.0.117
                               rust-system-deps-7.0.7
                               rust-target-lexicon-0.13.3
                               rust-tempfile-3.26.0
                               rust-thiserror-1.0.69
                               rust-thiserror-2.0.18
                               rust-thiserror-impl-1.0.69
                               rust-thiserror-impl-2.0.18
                               rust-thread-local-1.1.9
                               rust-tiff-0.10.3
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
                               rust-typenum-1.19.0
                               rust-udev-0.9.3
                               rust-unicode-ident-1.0.24
                               rust-unicode-segmentation-1.12.0
                               rust-unicode-xid-0.2.6
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
                               rust-wayland-backend-0.3.12
                               rust-wayland-client-0.31.12
                               rust-wayland-csd-frame-0.3.0
                               rust-wayland-cursor-0.31.12
                               rust-wayland-egl-0.32.9
                               rust-wayland-protocols-0.32.10
                               rust-wayland-protocols-misc-0.3.10
                               rust-wayland-protocols-plasma-0.3.10
                               rust-wayland-protocols-wlr-0.3.10
                               rust-wayland-scanner-0.31.8
                               rust-wayland-server-0.31.11
                               rust-wayland-sys-0.31.8
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
                               rust-windows-sys-0.52.0
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
                               rust-winit-0.30.12
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
                               rust-zerocopy-0.8.39
                               rust-zerocopy-derive-0.8.39
                               rust-zmij-1.0.21
                               rust-zune-core-0.4.12
                               rust-zune-core-0.5.1
                               rust-zune-inflate-0.2.54
                               rust-zune-jpeg-0.4.21
                               rust-zune-jpeg-0.5.12)))
