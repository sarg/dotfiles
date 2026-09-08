(define-module (personal packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (personal packages binary)
  #:use-module (personal packages ghostty)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pdf))

;;; https://codeberg.org/divyaranjan/divya-lambda/src/branch/master/divya-lambda/packages/emacs-xyz.scm
(define-public emacs-reader
  (package
    (name "emacs-reader")
    (properties '((commit . "a0e3615adbf520a5743bbbfd7da6d2bb8478b30b")))
    (version (git-version "0.3.2" "9" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://codeberg.org/divyaranjan/emacs-reader")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012i36l5wgzw4mbxr2knb96f7x95qfg0pqa7dfam23fmsfw57fy0"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f                      ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'expand-load-path 'build-module
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "make" "USE_PKGCONFIG=no}"))) ; We don't need pkg-config
          (add-after 'install 'install-module
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (target-dir (string-append out
                                                "/share/emacs/site-lisp/" #$name "-" #$version)))
                (install-file "render-core.so" target-dir)))))))

    (native-inputs (list mupdf gcc))
    (home-page "https://codeberg.org/divyaranjan/emacs-reader")
    (synopsis
     "An all-in-one document reader for all formats in Emacs, backed by MuPDF.")
    (description
     "An all-in-one document reader for GNU Emacs, supporting all major document formats.
This package intends to take from doc-view, nov.el, and pdf-tools and make them better.
And as such, it is effectively a drop-in replacement for them.")
    (license license:gpl3+)))

(define-public emacs-torrent-mode
  (package
    (name "emacs-torrent-mode")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sarg/torrent-mode.el")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ajmdz6vk68diyp93q0jbmwjwrwmdyszya7yppsn2qdpiksgz1z5"))))
    (build-system emacs-build-system)
    (inputs (list emacs-aria2))
    (propagated-inputs (list emacs-tablist emacs-bencoding))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/sarg/torrent-mode.el")
    (synopsis "Display torrent files in a tabulated view")
    (description "This package displays torrent files using tablist-mode.
A helper is provided to download files using @code{aria2}.")
    (license license:unlicense)))

(define-public emacs-aria2
  (package
    (name "emacs-aria2")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sarg/aria2")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "005h0d7qbfhj1ny8nbsprfqhnl400sfqkj632xykcigbhr6h61ss"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/ukaszg/aria2")
    (synopsis "Control @code{aria2c} commandline tool from Emacs")
    (description "This package provides a tabulated-list based interface to aria2 bittorent
client.")
    (license license:unlicense)))

(define-public emacs-iwd-manager
  (package
    (name "emacs-iwd-manager")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sarg/wifi-manager")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "110p1hbpzzi0fimkibw4ac8vxxim2piaa1063badxkcnkwr5zs8l"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f
       #:include '("^iwd-manager")))
    (propagated-inputs (list emacs-promise))
    (home-page "https://github.com/sarg/wifi-manager")
    (synopsis "Manage IWD via the D-Bus interface")
    (description
     "This package provides a dbus-based client for @code{iNet} Wireless Daemon.
Supports connecting to PSK networks.")
    (license license:unlicense)))

(define-public emacs-emms-player-spotify
  (package
    (name "emacs-emms-player-spotify")
    (version "20260125")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sarg/emms-spotify")
              (commit "43e6d0421cb622a21258d48eebb7070c6d9bc85b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ll4f4llbw6x9v9kxycy81kxxa89j54rqa40lld5qbf0sxv51xya"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat emacs-emms emacs-s
                             emacs-request emacs-consult))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'contrib
            (lambda _
              (copy-file "contrib/consult-spotify-emms.el"
                         "consult-spotify-emms.el"))))))
    (home-page "https://github.com/sarg/emms-spotify")
    (synopsis "Spotify player for EMMS")
    (description
     "This package provides an EMMS player wrapper for Spotify.  It supports two types
of links: internal spotify ids in form of \"spotify:<type>:<id>\" and in form of a
\"https://open.spotify.com/<type>/<id>\" URLs.  The package delegates actual
playback to the desktop app, which must be already running.  For proper work,
please disable Autoplay feature in the desktop app, so that EMMS would have full
control over the playback queue.  As the package uses DBUS MPRIS interface to
control the player, it will work only on platforms where dbus is available.")
    (license #f)))

(define-public emacs-dictcc
  (package
    (name "emacs-dictcc")
    (version "20221231.1703")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/martenlienen/dictcc.el")
              (commit "30b505759e5a97c2aaa8b0e8ea5e187fdf625c65")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wwmmfyzdqaixsg75jlhwjy09cld0gvvdmnnl0951ivzsm0g0dy0"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/martenlienen/dictcc.el")
    (synopsis "Look up translations on dict.cc")
    (description
     "Look up translations on dict.cc.  Then you can browse and pick one of them and
insert it at point.")
    (license #f)))

(define-public emacs-powerthesaurus
  (package
    (name "emacs-powerthesaurus")
    (version "20230426.1719")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/SavchenkoValeriy/emacs-powerthesaurus")
              (commit "4b97797cf789aaba411c61a85fe23474ebc5bedc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19bd8rwjwprxp54vy1a53m2gv138ybda5ybxvm6q7msqhxmphf3g"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-jeison emacs-s))
    (arguments '(#:tests? #f))
    (home-page "http://github.com/SavchenkoValeriy/emacs-powerthesaurus")
    (synopsis "Powerthesaurus integration")
    (description
     "; This package is an integration with powerthesaurus.org. ; It helps to look up
a word in powerthesaurus and either replace or ; insert selected option in the
buffer (depending on the current selection).")
    (license #f)))

(define-public emacs-greymatters-theme
  (package
    (name "emacs-greymatters-theme")
    (version "20150621.1123")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mswift42/greymatters-theme")
              (commit "a7220a8c6cf18ccae2b76946b6f01188a7c9d5d1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14c09m9p6556rrf0qfad4zsv7qxa5flamzg6fa83cxh0qfg7wjbp"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/mswift42/greymatters-theme")
    (synopsis "Emacs 24 theme with a light background")
    (description
     "This package lacks a description.  Run \"info '(guix) Synopses and Descriptions'\" for more information.")
    (license #f)))

(define-public emacs-darkman
  (package
    (name "emacs-darkman")
    (version "20241019.1404")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/darkman-" version
                           ".tar"))
       (sha256
        (base32 "0dk7cscjd6dvmw6rzivh3zq9cmk0a8d4ayy7irrxhqxfdmyidl99"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://darkman.grtcdr.tn")
    (synopsis "Seamless integration with Darkman")
    (description
     "darkman.el provides seamless integration between Darkman and Emacs using the
D-Bus protocol.")
    (license #f)))

(define-public emacs-org-projectile
  (package
    (name "emacs-org-projectile")
    (version "20230817.851")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/colonelpanic8/org-project-capture")
              (commit "bf1c30b750020ab8dd634dd66b2c7b76c56286c5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wvw5y5s37p9j0m2ljp7n1s1casbhiyrcnfpvdghvdd0fk8wcybp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-projectile emacs-dash
                             emacs-org-project-capture
                             emacs-org-category-capture))
    (arguments
     '(#:tests? #f #:include '("^org-projectile.el$")
       #:exclude '()))
    (home-page "https://github.com/colonelpanic8/org-project-capture")
    (synopsis
     "Repository todo capture and management for org-mode with projectile")
    (description
     "This package provides an easy interface to creating per project org-mode TODO
headings, whether in a single file, or in a file stored in each project
directory.")
    (license #f)))

(define-public emacs-org-tidy
  (package
    (name "emacs-org-tidy")
    (version "20241212.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jxq0/org-tidy")
              (commit "0bea3a2ceaa999e0ad195ba525c5c1dcf5fba43b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rwq53j31vixyhsi7khb1xc0fcqdmqyp7ycq5hinligfxk87sr4s"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/jxq0/org-tidy")
    (synopsis "A minor mode to tidy org-mode buffers")
    (description
     "This package provides a minor mode to tidy org-mode buffers.")
    (license #f)))

(define-public emacs-german-holidays
  (package
    (name "emacs-german-holidays")
    (version "20181213.644")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rudolfochrist/german-holidays")
              (commit "a8462dffccaf2b665f2032e646b5370e993a386a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rf8p42pl7jmmdiibfcamlbr3kg6kslffv8vbpwn20xm2ii13rxz"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/rudolfochrist/german-holidays")
    (synopsis "German holidays for Emacs calendar")
    (description
     "Installation: To use `german-holidays exclusively (setq calendar-holidays
holiday-german-holidays) To use german-holidays additionally (setq
calendar-holidays (append calendar-holidays holiday-german-holidays)) If you'd
like to show holidays for Rhineland Palatinate only, you can use (setq
calendar-holidays holiday-german-RP-holidays) This works for for all states:
`holiday-german-BW-holidays `holiday-german-HE-holidays
`holiday-german-HH-holidays etc. ; Credits inspired by
https://github.com/abo-abo/netherlands-holidays.")
    (license #f)))

(define-public emacs-justl
  (package
    (name "emacs-justl")
    (version "20251111.948")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/psibi/justl.el")
              (commit "3b11dd8ac7ebeaca5da6c80223254a9f0494b275")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i5m8iqyw7pkc2cjkk6z0px6lqm0w0ad9r1f9i8dhi8v8v7lk70r"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient emacs-s emacs-f emacs-inheritenv))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/psibi/justl.el")
    (synopsis "Major mode for driving just files")
    (description
     "Emacs extension for driving just files To list all the recipes present in your
justfile, call M-x justl You don't have to call it from the actual justfile.
Calling it from the directory where the justfile is present should be enough.
Alternatively, if you want to just execute a recipe, call M-x
justl-exec-recipe-in-dir To execute default recipe, call
justl-exec-default-recipe Shortcuts: On the just screen, place your cursor on a
recipe h => help popup ? => help popup g => refresh e => execute recipe E =>
execute recipe with a shell w => execute recipe with arguments W => open a shell
without executing Customize: By default, justl searches the executable named
`just`, you can change the `justl-executable` variable to set any explicit path.
 You can also control the width of the RECIPE column in the justl buffer via
`justl-recipe width`.  By default it has a value of 20.  You can change the
shell between `eshell and `vterm using the `justl-shell variable.  Using vterm
requires the `vterm package to be installed.")
    (license #f)))

(define-public emacs-eshell-atuin
  (package
    (name "emacs-eshell-atuin")
    (version "20260222.802")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/SqrtMinusOne/eshell-atuin")
              (commit "142536a01a9d6d92d802e41474c290e0ca655deb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g4v9l755gvclggv33mk13vn7vw8sxivi3xgcmbgav1svim01v6q"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/SqrtMinusOne/eshell-atuin")
    (synopsis "Integrate eshell with atuin, a shell history tool")
    (description
     "Integrate `eshell with atuin <https://github.com/atuinsh/atuin> atuin stores
shell history in a database, which allows for having same history across
multiple shells, sessions, and optionally across different machines.  This
package provides functionality to store and browse eshell history in atuin.
`eshell-atuin-mode and `eshell-atuin-history are the corresponding entrypoints.
See also the package README at
<https://github.com/@code{SqrtMinusOne/eshell-atuin>}.")
    (license license:gpl3)))

(define-public emacs-aidermacs
  (package
    (name "emacs-aidermacs")
    (version "20251203.2318")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/MatthewZMD/aidermacs")
              (commit "6d0c41d1cfd24821fb32933edf8c0c2a9bb8c847")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mwh2ikw3kkbphm2f8grgygmib51azwisp5s7nljb17aq7ncdk3h"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient emacs-compat emacs-markdown-mode))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/MatthewZMD/aidermacs")
    (synopsis "AI pair programming with Aider")
    (description
     "Aidermacs integrates with Aider (https://aider.chat/) for AI-assisted code
modification in Emacs.  Aider lets you pair program with LLMs to edit code in
your local git repository.  It works with both new projects and existing code
bases, supporting Claude, @code{DeepSeek}, @code{ChatGPT}, and can connect to
almost any LLM including local models.  Think of it as having a helpful coding
partner that can understand your code, suggest improvements, fix bugs, and even
write new code for you.  Whether you're working on a new feature, debugging, or
just need help understanding some code, Aidermacs provides an intuitive way to
collaborate with AI while staying in your familiar Emacs environment.
Originally forked from Kang Tu <tninja@@gmail.com>'s Aider.el.")
    (license #f)))

(define-public emacs-hnreader
  (package
    (name "emacs-hnreader")
    (version "20250703.328")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/thanhvg/emacs-hnreader")
              (commit "a56f67a99a855ca656da1c1985e09f44509e4bbb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1abqjrzq75ijhn3sfmy0wy6acp8x7nj5gihqy34mickz4v5wqbil"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-promise emacs-request emacs-org))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/thanhvg/emacs-hnreader/")
    (synopsis "A hackernews reader")
    (description
     "This package renders hackernews website at https://news.ycombinator.com/ in an
org buffer.  Almost everything works.  Features that are not supported are
account related features.  You cannot add comment, downvote or upvote. ;
Dependencies `promise and `request are required.  user must have `org-mode 9.2
or later installed also. ; Commands hnreader-news: Load news page.
hnreader-past: Load past page.  hnreader-ask: Load ask page.  hnreader-show:
Load show page.  hnreader-newest: Load new link page.  hnreader-best: Load page
with best articles.  hnreader-more: Load more.  hnreader-back: Go back to
previous page.  hnreader-comment: read an HN item url such as
https://news.ycombinator.com/item?id=1 ; Customization hnreader-history-max: max
number history items to remember.  hnreader-view-comments-in-same-window: if nil
then will not create new window when viewing comments ; Changelog 0.2.8
2025-07-02 Show title in all pages 0.2.7 2025-07-01 update title capture for
page and item 0.2.6 2024-11-09 update css class capture 0.2.5 2022-11-16 handle
all kinds of items 0.2.4 2022-11-16 add reply link 0.2.3 2022-11-14 add reply
link 0.2.2 2022-09-27 update css class grab for entry title 0.2.1 2021-10-18
update css class grab for entry title.")
    (license #f)))

(define-public emacs-bencoding
  (package
    (name "emacs-bencoding")
    (version "20200331.1102")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/xuchunyang/bencoding.el")
              (commit "1e16ccfd5c6560a83ae2926afe4a5076a541d3d6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dgwh3z1ni619kxpdxv8r2k0jhgj5h6ssxp6l8s26mhpmy1bkm6c"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/xuchunyang/bencoding.el")
    (synopsis "Bencoding decoding and encoding")
    (description "An Emacs Lisp library for reading and writing Bencoding
<https://en.wikipedia.org/wiki/Bencode>.")
    (license #f)))

(define-public emacs-jeison
  (package
    (name "emacs-jeison")
    (version "20190721.1651")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/SavchenkoValeriy/jeison")
              (commit "19a51770f24eaa7b538c7be6a8a5c25d154b641f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ipmh2zg1pffpkk00wr2d8s3g51bnv3kmnci8g79i7vnm3i4my85"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash))
    (arguments '(#:tests? #f))
    (home-page "http://github.com/SavchenkoValeriy/jeison")
    (synopsis "A library for declarative JSON parsing")
    (description
     "Jeison is a library for transforming JSON objects (or `alist's) into EIEIO
objects.")
    (license #f)))

(define-public emacs-org-project-capture
  (package
    (name "emacs-org-project-capture")
    (version "20230830.1733")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/colonelpanic8/org-project-capture")
              (commit "bf1c30b750020ab8dd634dd66b2c7b76c56286c5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wvw5y5s37p9j0m2ljp7n1s1casbhiyrcnfpvdghvdd0fk8wcybp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-s emacs-org-category-capture))
    (arguments
     '(#:tests? #f #:include '("^org-project-capture.el$"
                               "^org-project-capture-backend.el$")
       #:exclude '()))
    (home-page "https://github.com/colonelpanic8/org-project-capture")
    (synopsis "Repository todo capture and management for org-mode")
    (description
     "This package provides an easy interface to creating per project org-mode TODO
headings, whether in a single file, or in a file stored in each project
directory.")
    (license #f)))

(define-public emacs-org-category-capture
  (package
    (name "emacs-org-category-capture")
    (version "20230830.1733")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/colonelpanic8/org-project-capture")
              (commit "bf1c30b750020ab8dd634dd66b2c7b76c56286c5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wvw5y5s37p9j0m2ljp7n1s1casbhiyrcnfpvdghvdd0fk8wcybp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org))
    (arguments
     '(#:tests? #f #:include '("^org-category-capture[^/]*.el$")
       #:exclude '()))
    (home-page "https://github.com/IvanMalison/org-project-capture")
    (synopsis "Contextualy capture of org-mode TODOs")
    (description
     "This package provides an interface that can be used to capture TODOs with a
category that is selected depending on a some piece of Emacs context.")
    (license #f)))

(define-public emacs-qutebrowser
  (package
    (name "emacs-qutebrowser")
    (version "20260101.840")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lrustand/qutebrowser.el")
              (commit "00d9a306d25fb5a87ff4dd600af3023449e7f172")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j9fiz7spcmvbqzlngfxy4p3d9rzqmiv8zazqgm296019hmfhmwm"))))
    (build-system emacs-build-system)
    (inputs (list emacs-consult emacs-doom-modeline emacs-exwm emacs-evil
                  emacs-password-store emacs-password-store-otp))
    (arguments (list #:tests? #f #:emacs emacs))
    (home-page "https://github.com/lrustand/qutebrowser.el")
    (synopsis "Glue between qutebrowser and EXWM")
    (description "qutebrowser.el is an Emacs package that provides tight two-way
integration between Qutebrowser and EXWM")
    (license license:gpl3+)))

(define-public emacs-cyclekey
  (package
    (name "emacs-cyclekey")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/shankar2k/cyclekey")
              (commit "e5635b1fe9d133afeada146656359d6000607bdf")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zmh41gqpyb1p4jxwiy39f1pdziq4z57cnamxvl0gj2m7bj5q0c3"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/shankar2k/cyclekey")
    (synopsis "Enter diacritics and accents easily")
    (description "This package provides the command ~cyclekey-cycle~ which cycles through relevant
diacritics and accents for the character at point")
    (license license:gpl3+)))

(define-public emacs-corg
  (package
    (name "emacs-corg")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/isamert/corg.el")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hkg13kpwj4hljvyhs369j99jkp206x7j670w8znfrdjrv75jqmq"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-s))
    (arguments (list #:tests? #f))
    (home-page "https://github.com/isamert/corg.el")
    (synopsis "Auto-completion for org-mode source block header")
    (description "Emacs package that provides completion-at-point for
Org-mode source block and dynamic block headers.")
    (license license:gpl3+)))

(define-public emacs-tramp-hlo
  (package
    (name "emacs-tramp-hlo")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/tramp-hlo-" version
                           ".tar"))
       (sha256
        (base32 "1bs3wz644ibc332nxzf880zklmwsfwhlimdvamas3568ns21xqn0"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-tramp))
    (home-page "https://github.com/jsadusk/tramp-hlo")
    (synopsis "High level operations as Tramp handlers")
    (description "No description available.")
    (license license:gpl3+)))

(define-public emacs-visual-shorthands
  (package
    (name "emacs-visual-shorthands")
    (version "20260104.2221")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/gggion/visual-shorthands.el")
              (commit "0511154773533ec2e3c25efa5515ea548ee7e9e1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x7abjfmb492bv9y1s0pkc55dl57gbx9sbrdbv37k839s8nj9lpk"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/gggion/visual-shorthands.el")
    (synopsis "Visual abbreviations for symbol prefixes")
    (description
     "Replace long prefixes with short ones visually using overlays.  Example:
\"application-config-manager--\" -> \"acm:\" Basic usage:
(visual-shorthands-add-mapping \"application-config-manager--\" \"acm:\")
(visual-shorthands-mode 1) Abbreviates PREFIXES only, not whole symbols.")
    (license license:gpl3+)))

(define-public emacs-reddigg
  (package
    (name "emacs-reddigg")
    (version "20260818")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/thanhvg/emacs-reddigg.git")
               (commit "307b95026da1cb2450d153e0b77eb26d0d5a0980")))
        (sha256
          (base32
            "034gihaxr9i856gwl8mx347apv66ayvqqidx73dfvncmqy66dby6"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs (list emacs-promise emacs-ht))
    (home-page "https://github.com/thanhvg/emacs-reddigg")
    (synopsis "A reader for redditt")
    (description "Documentation at https://melpa.org/#/reddigg")
    (license #f)))

(define-public emacs-kdeconnect
  (package
    (name "emacs-kdeconnect")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/carldotac/kdeconnect.el")
             (commit "daee28249b852cc52f4f32d35704a97af5cc4b7d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bavmbjj06ymw54459gpf5zglk3n9g71nngw5bpx18dhkr0n6gb8"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/carldotac/kdeconnect.el")
    (synopsis "An interface for KDE Connect")
    (description
     "This package provides helper functions to use the command line version of KDE
Connect, a bridge between Android devices and computers, without leaving the
comfort of Emacs.  It requires KDE Connect on your computer(s) and Android
device(s).  KDE Connect currently requires Linux on the desktop, but does not
require KDE.")
    (license #f)))

(define-public emacs-pipewire
  (package
    (name "emacs-pipewire")
    (version "20220725.1858")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.zamazal.org/pdm/pipewire-0")
             (commit "ae7a95230f102e7430a80acb02850bc24430c3b2")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f4hbjh5jb1skk104s52brq9mgsl275g1l631x07yffdps310axr"))))
    (build-system emacs-build-system)
    (home-page "https://git.zamazal.org/pdm/pipewire-0")
    (synopsis "PipeWire user interface")
    (description
     "@code{PipeWire} user interface and library.  It currently uses pw-cli and
pw-metadata command line utilities to interact with @code{PipeWire}.  An
interactive @code{PipeWire} buffer can be displayed using `M-x pipewire'.  There
you can view basic @code{PipeWire} status and change some settings.
`pipewire-increase-volume', `pipewire-decrease-volume and `pipewire-toggle-muted
functions can be used also standalone and are suitable to bind on the multimedia
keys.  The package can be used also non-interactively in Elisp programs.  See
pipewire-lib.el source file for available functions.")
    (license #f)))

(define-public emacs-clutch
  (package
    (name "emacs-clutch")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LuciusChen/clutch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cyjawbrfphdqcrwg10nrhcvmr45hlhjr3v1g8k0ypcgmfa7cmjs"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient))
    (home-page "https://github.com/LuciusChen/clutch")
    (synopsis "Interactive database client")
    (description
     "Interactive database client with native and JDBC backends.  Provides: -
`clutch-mode': SQL editing major mode (derived from `sql-mode') - `clutch-repl':
REPL via `comint-mode - Query execution with horizontally scrollable result
tables - Object discovery and completion Entry points: M-x clutch-mode — open a
SQL editing buffer M-x clutch-repl — open a REPL Open a .mysql file — activates
clutch-mode automatically.")
    (license license:gpl3)))

(define-public emacs-browsel
  (package
    (name "emacs-browsel")
    (version "20260726.1635")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmgerman/browsel")
             (commit "d65e53c7f88ab24bc405ec7752afb82f803cd9ac")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w91kxvjs3q1wn4xhgfidpmazwb9mr2cjf92r555snyxk80rr170"))))
    (arguments (list #:tests? #f))                      ;no tests
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-websocket emacs-org emacs-vertico))
    (home-page "https://github.com/dmgerman/browsel")
    (synopsis "WebSocket bridge to a Chrome/Firefox extension")
    (description
     "This package provides a local @code{WebSocket} server that exchanges JSON frames
with a Chrome (MV3) extension.")
    (license license:gpl3)))

(define-public mxp
  (package
    (name "mxp")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/agzam/mxp")
              (commit "c4b0ecb48ef52a3f1839580bbe9654f72bcb402b")))
       (sha256
        (base32 "0bp0afs7gwz5wn8fixik9gznfs91my2aybbgml88qbdy1pk62ngz"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~`(("mxp" "bin/"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'wrap-binary
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (string-append (assoc-ref outputs "out")
                                                         "/bin/mxp")
                              `("PATH" ":" prefix
                                (,(dirname (search-input-file inputs "bin/base64"))
                                 ,(dirname (search-input-file inputs "bin/grep"))
                                 ,(dirname (search-input-file inputs "bin/sed"))))))))))
    (inputs (list coreutils grep sed))
    (home-page "https://github.com/agzam/mxp")
    (synopsis "Pipe content between your terminal and Emacs buffers")
    (description "mxp (Emacs Piper) is a shell script that acts as a bridge between Unix pipes and Emacs buffers.")
    (license license:unlicense)))

(define-public emacs-evil-ghostel
  (package
    (inherit emacs-ghostel)
    (name "emacs-evil-ghostel")
    (propagated-inputs (list emacs-evil emacs-ghostel))
    (native-inputs '())
    (arguments
     '(#:tests? #f
       #:lisp-directory "extensions/evil-ghostel"
       #:phases
       (modify-phases %standard-phases
         (delete 'patch-el-files))))
    (home-page "https://github.com/dakra/ghostel")
    (synopsis "Evil-mode integration for ghostel")
    (description "Evil-mode integration for the ghostel terminal emulator")
    (license license:gpl3)))
