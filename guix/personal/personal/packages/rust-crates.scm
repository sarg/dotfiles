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

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-annotate-snippets-0.9.2
  (crate-source "annotate-snippets" "0.9.2"
                "07p8r6jzb7nqydq0kr5pllckqcdxlyld2g275v425axnzffpxbyc"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-arc-swap-1.7.1
  (crate-source "arc-swap" "1.7.1"
                "0mrl9a9r9p9bln74q6aszvf22q1ijiw089jkrmabfqkbj31zixv9"))

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

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-backtrace-0.3.75
  (crate-source "backtrace" "0.3.75"
                "00hhizz29mvd7cdqyz5wrj98vqkihgcxmv2vl7z0d0f53qrac1k8"))

(define rust-base62-2.2.3
  (crate-source "base62" "2.2.3"
                "048i72spz3pvp6n9q4xb8bibb4hsd5qk5pfyjfb4f9vfg1argpqs"))

(define rust-bindgen-0.69.5
  (crate-source "bindgen" "0.69.5"
                "1240snlcfj663k04bjsg629g4wx6f83flgbjh5rzpgyagk3864r7"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

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

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-cc-1.2.38
  (crate-source "cc" "1.2.38"
                "1sg7gd94611qhryvb0iip0zibjnhf1yha2wnp0pw2mgrd3himx40"))

(define rust-cexpr-0.6.0
  (crate-source "cexpr" "0.6.0"
                "0rl77bwhs5p979ih4r0202cn5jrfsrbgrksp40lkfz5vk1x3ib3g"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.15.8
  (crate-source "cfg-expr" "0.15.8"
                "00lgf717pmf5qd2qsxxzs815v6baqg38d6m5i6wlh235p14asryh"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-clang-sys-1.8.1
  ;; TODO: Check bundled sources.
  (crate-source "clang-sys" "1.8.1"
                "1x1r9yqss76z8xwpdanw313ss6fniwc1r7dzb5ycjn0ph53kj0hb"))

(define rust-clap-3.2.25
  (crate-source "clap" "3.2.25"
                "08vi402vfqmfj9f07c4gl6082qxgf4c9x98pbndcnwbgaszq38af"))

(define rust-clap-derive-3.2.25
  (crate-source "clap_derive" "3.2.25"
                "025hh66cyjk5xhhq8s1qw5wkxvrm8hnv5xwwksax7dy8pnw72qxf"))

(define rust-clap-lex-0.2.4
  (crate-source "clap_lex" "0.2.4"
                "1ib1a9v55ybnaws11l63az0jgz5xiy24jkdgsmyl7grcm3sz4l18"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-convert-case-0.6.0
  (crate-source "convert_case" "0.6.0"
                "1jn1pq6fp3rri88zyw6jlhwwgf6qiyc08d6gjv0qypgkl862n67c"))

(define rust-cookie-factory-0.3.3
  (crate-source "cookie-factory" "0.3.3"
                "18mka6fk3843qq3jw1fdfvzyv05kx7kcmirfbs2vg2kbw9qzm1cq"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-custom-debug-0.6.2
  (crate-source "custom_debug" "0.6.2"
                "13i9cldd9glg8k25z5ll5wb083rnl07pl7bzhwgf3cv7jnnx39rd"))

(define rust-custom-debug-derive-0.6.2
  (crate-source "custom_debug_derive" "0.6.2"
                "04cw7wqf42rjnah5b79vpwawh94m5rjjbrrb9xicgxjjhvdcw1x7"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

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

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

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

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

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

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-glob-0.3.3
  (crate-source "glob" "0.3.3"
                "106jpd3syfzjfj2k70mwm0v436qbx96wig98m4q8x071yrq35hhc"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-globwalk-0.8.1
  (crate-source "globwalk" "0.8.1"
                "1k6xwkydr7igvwjn3xkwjywk4213lcs53f576ilqz1h84jaazqwk"))

(define rust-hashbrown-0.12.3
  (crate-source "hashbrown" "0.12.3"
                "1268ka4750pyg2pbgsr43f0289l5zah4arir2k4igx5a8c6fg7la"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-heck-0.4.1
  (crate-source "heck" "0.4.1"
                "1a7mqsnycv5z4z5vnv1k34548jzmc0ajic7c1j8jsaspnhw5ql4m"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-indexmap-1.9.3
  (crate-source "indexmap" "1.9.3"
                "16dxmy7yvk51wvnih3a3im6fp5lmx0wx76i03n06wyak6cwhw1xx"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-io-uring-0.7.10
  (crate-source "io-uring" "0.7.10"
                "0yvjyygwdcqjcgw8zp254hvjbm7as1c075dl50spdshas3aa4vq4"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-itertools-0.11.0
  (crate-source "itertools" "0.11.0"
                "0mzyqcc59azx9g5cg6fs8k529gvh4463smmka6jvzs3cd2jp7hdi"))

(define rust-itertools-0.12.1
  (crate-source "itertools" "0.12.1"
                "0s95jbb3ndj1lvfxyq5wanc0fm0r6hg6q4ngb92qlfdxvci10ads"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-iwdrs-0.1.6
  (crate-source "iwdrs" "0.1.6"
                "13yc1jbw43hgh7gwyx2yj47j6bfn59h8a7dpyr00pr1djl9kf5x1"))

(define rust-jiff-0.2.15
  (crate-source "jiff" "0.2.15"
                "0jby6kbs2ra33ji0rx4swcp66jzmcvgszc5v4izwfsgbn6w967xy"))

(define rust-jiff-static-0.2.15
  (crate-source "jiff-static" "0.2.15"
                "1d4l4pvlhz3w487gyhnzvagpbparspv4c8f35qk6g5w9zx8k8d03"))

(define rust-js-sys-0.3.80
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.80"
                "0bkhnbna0a9sqhhswfar0mzi8mpy2dygv4zbzfdbm97bqnz16bw5"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-lazycell-1.3.0
  (crate-source "lazycell" "1.3.0"
                "0m8gw7dn30i0zjjpjdyf6pc16c34nl71lpv461mix50x3p70h3c3"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libdbus-sys-0.2.6
  ;; TODO: Check bundled sources.
  (crate-source "libdbus-sys" "0.2.6"
                "17xx4dy30fn81zhwsm4y2c84wr0apyiams8hy20lc3mmzrp8bgjw"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libspa-0.8.0
  (crate-source "libspa" "0.8.0"
                "044qs48yl0llp2dmrgwxj9y1pgfy09i6fhq661zqqb9a3fwa9wv5"))

(define rust-libspa-sys-0.8.0
  ;; TODO: Check bundled sources.
  (crate-source "libspa-sys" "0.8.0"
                "07yh4i5grzbxkchg6dnxlwbdw2wm5jnd7ffbhl77jr0388b9f3dz"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-mac-notification-sys-0.6.6
  ;; TODO: Check bundled sources.
  (crate-source "mac-notification-sys" "0.6.6"
                "1m0wl0rprmrjbv47h0vwff3a2isq8wkddagdr521pxja1288970i"))

(define rust-macaddr-1.0.1
  (crate-source "macaddr" "1.0.1"
                "1n5jxn79krlql810c4w3hdkvyqc01141dc5y6fr9sxff2yy0pvms"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

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

(define rust-normpath-1.5.0
  (crate-source "normpath" "1.5.0"
                "16z68q809749ky2vl72f3lqnhf3vjclvcc3y2z5v8m2nj0msn8xz"))

(define rust-notify-rust-4.11.7
  (crate-source "notify-rust" "4.11.7"
                "0024xqbn29z1k6cfbi8w7c1p73hscqwkpbwlwwa2bam5cn328hk4"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-derive-0.4.2
  (crate-source "num-derive" "0.4.2"
                "00p2am9ma8jgd2v6xpsz621wc7wbn1yqi71g15gc3h67m7qmafgd"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-objc2-0.6.2
  (crate-source "objc2" "0.6.2"
                "1g3qa1vxp6nlh4wllll921z299d3s1is31m1ccasd8pklxxka7sn"))

(define rust-objc2-core-foundation-0.3.1
  (crate-source "objc2-core-foundation" "0.3.1"
                "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.1
  (crate-source "objc2-foundation" "0.3.1"
                "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-str-bytes-6.6.1
  (crate-source "os_str_bytes" "6.6.1"
                "1885z1x4sm86v5p41ggrl49m58rbzzhd1kj72x46yy53p62msdg2"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

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

(define rust-pipewire-sys-0.8.0
  ;; TODO: Check bundled sources.
  (crate-source "pipewire-sys" "0.8.0"
                "04hiy3rl8v3j2dfzp04gr7r8l5azzqqsvqdzwa7sipdij27ii7l4"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

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

(define rust-process-wrap-8.2.1
  (crate-source "process-wrap" "8.2.1"
                "189vzjn8dan18cnb0qlk3b472a6imji8wqlzxj13mwi20hplzvx3"))

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-regex-1.11.2
  (crate-source "regex" "1.11.2"
                "04k9rzxd11hcahpyihlswy6f1zqw7lspirv4imm4h0lcdl8gvmr3"))

(define rust-regex-automata-0.4.10
  (crate-source "regex-automata" "0.4.10"
                "1mllcfmgjcl6d52d5k09lwwq9wj5mwxccix4bhmw5spy1gx5i53b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

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

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-serde-1.0.226
  (crate-source "serde" "1.0.226"
                "1zcm2asp9fiphbp0k96whabw02kiiqgzxhbyz85vc92v088n9jhd"))

(define rust-serde-core-1.0.226
  (crate-source "serde_core" "1.0.226"
                "1936x6cpqgyq57nm7qi416dsc4fiq3jv6d7vh74xmfgdk4wscaxs"))

(define rust-serde-derive-1.0.226
  (crate-source "serde_derive" "1.0.226"
                "0cyvkilp34an3f90b0idw0jjsyq20h7v47gsp8qkfmrl5zi3mdcd"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-yaml-0.9.34+deprecated
  (crate-source "serde_yaml" "0.9.34+deprecated"
                "0isba1fjyg3l6rxk156k600ilzr8fp7crv82rhal0rxz5qd1m2va"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-0.3.18
  (crate-source "signal-hook" "0.3.18"
                "1qnnbq4g2vixfmlv28i1whkr0hikrf1bsc4xjy2aasj2yina30fq"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strsim-0.10.0
  (crate-source "strsim" "0.10.0"
                "08s69r4rcrahwnickvi0kq49z524ci50capybln83mg6b473qivk"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

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

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sys-locale-0.3.2
  (crate-source "sys-locale" "0.3.2"
                "1i16hq9mkwpzqvixjfy1ph4i2q5klgagjg4hibz6k894l2crmawf"))

(define rust-system-deps-6.2.2
  (crate-source "system-deps" "6.2.2"
                "0j93ryw031n3h8b0nfpj5xwh3ify636xmv8kxianvlyyipmkbrd3"))

(define rust-target-lexicon-0.12.16
  (crate-source "target-lexicon" "0.12.16"
                "1cg3bnx1gdkdr5hac1hzxy64fhw4g7dqkd0n3dxy5lfngpr1mi31"))

(define rust-tauri-winrt-notification-0.7.2
  (crate-source "tauri-winrt-notification" "0.7.2"
                "1fd9gcllx1rkp9h1ppq976bhqppnil5xsy36li1zx2g4gph6c7hb"))

(define rust-tempfile-3.22.0
  (crate-source "tempfile" "3.22.0"
                "0lza9r7dzm4k9fghw24yql6iz59wq8xgs46a7i29ir6xz88lvyl4"))

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

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

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

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-write-0.1.2
  (crate-source "toml_write" "0.1.2"
                "008qlhqlqvljp1gpp9rn5cqs74gwvdgbvs92wnpq8y3jlz4zi6ax"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-triomphe-0.1.14
  (crate-source "triomphe" "0.1.14"
                "11fciha522hrz6pkafy3xlq20w405w9dqvy9ln7ba1s8v8k7g3zg"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-segmentation-1.12.0
  (crate-source "unicode-segmentation" "1.12.0"
                "14qla2jfx74yyb9ds3d2mpwpa4l4lzb9z57c6d2ba511458z5k7n"))

(define rust-unicode-width-0.1.14
  (crate-source "unicode-width" "0.1.14"
                "1bzn2zv0gp8xxbxbhifw778a7fc93pa6a1kj24jgg9msj07f7mkx"))

(define rust-unsafe-libyaml-0.2.11
  (crate-source "unsafe-libyaml" "0.2.11"
                "0qdq69ffl3v5pzx9kzxbghzn0fzn266i1xn70y88maybz9csqfk7"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

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

(define rust-wasm-bindgen-0.2.103
  (crate-source "wasm-bindgen" "0.2.103"
                "069qhf7yrl4jymzjzvwsmcmw96al639xim4scigpy5qapngsc45b"))

(define rust-wasm-bindgen-backend-0.2.103
  (crate-source "wasm-bindgen-backend" "0.2.103"
                "070x7fjnnvzm2y3a5j29wmss4z547cjdx3rnpixh19j56m105dqb"))

(define rust-wasm-bindgen-macro-0.2.103
  (crate-source "wasm-bindgen-macro" "0.2.103"
                "18481jkmczv4j0m747ypb0k1acq093hhbdhpb4sr856r27sg8rgw"))

(define rust-wasm-bindgen-macro-support-0.2.103
  (crate-source "wasm-bindgen-macro-support" "0.2.103"
                "0clsx611pday95s6wg8pndvrd8xknsaf20d40kk8x2irj6lh7h7z"))

(define rust-wasm-bindgen-shared-0.2.103
  (crate-source "wasm-bindgen-shared" "0.2.103"
                "1kx13fvmlxxaxf04vm3b14437hyq92zdy89pvcaclc54xzs3fg19"))

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

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

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

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

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

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-yansi-term-0.1.2
  (crate-source "yansi-term" "0.1.2"
                "1w8vjlvxba6yvidqdvxddx3crl6z66h39qxj8xi6aqayw2nk0p7y"))

(define rust-zbus-5.11.0
  (crate-source "zbus" "5.11.0"
                "1xxdxb95h4cyn8w03yrgrxrpy2pr9x7blqyfn9sy7f2z0dny81rd"))

(define rust-zbus-macros-5.11.0
  (crate-source "zbus_macros" "5.11.0"
                "1jpsvssaxh6fxpbb64dmfnrr8r05rsyfhm32bg63rva7r2lrgrsp"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zvariant-5.7.0
  (crate-source "zvariant" "5.7.0"
                "1nwqji0y214dnchq372rsfqzl86d9wgai909s761yay5ffzd77cr"))

(define rust-zvariant-derive-5.7.0
  (crate-source "zvariant_derive" "5.7.0"
                "13mnhlw8imn6garkqrq4gyxyj7rjnp0hfgqdv5mj4vd44q5zshv6"))

(define rust-zvariant-utils-3.2.1
  (crate-source "zvariant_utils" "3.2.1"
                "16g5id3h9q85c5vcwdfwkwmwzyladbr2q8x2xinr3xl95wa9v566"))

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
                                   rust-zvariant-utils-3.2.1)))
