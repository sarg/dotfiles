(define-module (personal packages sgtpuzzles)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (guix download))

(define-public halibut
  (package
    (name "halibut")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.chiark.greenend.org.uk/~sgtatham/halibut/halibut-" version "/halibut-" version ".tar.gz"))
       (sha256
        (base32 "0ciikn878vivs4ayvwvr63nnhpcg12m8023xv514zxqpdxlzg85a"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config perl))
    (arguments
     '(#:tests? #f)) ; No tests.
    (home-page "https://www.chiark.greenend.org.uk/~sgtatham/halibut/")
    (synopsis "Documentation production system for software manuals")
    (description "This is yet another text formatting system, intended primarily for writing
software documentation. It accepts a single source format and outputs any or all
of text, HTML, Windows Help, man pages, GNU info, PostScript, or PDF. It has
comprehensive indexing and cross-referencing support, and generates hyperlinks
within output documents wherever possible. It supports Unicode, with the ability
to fall back to an alternative representation when Unicode output is not
available.")

    (license license:expat)))

(define-public sgt-puzzles
  (package
    (name "sgt-puzzles")
    (version "20230313.adf2a09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/puzzles-" version ".tar.gz"))
       (sha256
        (base32 "0205wrshf2y27fzaqxv4gfpgmnlj4klwjcadwjrzjx7fkkzlr51j"))
       (patches (search-patches "install-two-icon-sizes.patch"))))
    (build-system cmake-build-system)
    (inputs (list gtk+ xdg-utils))
    (native-inputs (list pkg-config perl imagemagick halibut))
    (arguments
     `(#:tests? #f                      ; No tests.
       #:configure-flags '("-DNAME_PREFIX=sgt-")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'set-xdg-open-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "gtk.c"
                        (("(#define HELP_BROWSER_PATH ).+" all define)
                         (string-append define "\"" (search-input-file inputs "/bin/xdg-open") "\"" "\n")))
                      #t)))))
    (home-page "https://www.chiark.greenend.org.uk/~sgtatham/puzzles/")
    (synopsis "Simon Tatham's portable puzzle collection")
    (description "Simon Tatham's Portable Puzzle Collection contains a number of popular puzzle
games for one player. It currently consists of these games:
 * Black Box, ball-finding puzzle
 * Bridges, bridge-placing puzzle
 * Cube, rolling cube puzzle
 * Dominosa, domino tiling puzzle
 * Fifteen, sliding block puzzle
 * Filling, polyomino puzzle
 * Flip, tile inversion puzzle
 * Galaxies, symmetric polyomino puzzle
 * Guess, combination-guessing puzzle
 * Inertia, gem-collecting puzzle
 * Keen, arithmetic Latin square puzzle
 * Light Up, light-bulb placing puzzle
 * Loopy, loop-drawing puzzle
 * Magnets, magnet-placing puzzle
 * Map, map-colouring puzzle
 * Mines, mine-finding puzzle
 * Net, network jigsaw puzzle
 * Netslide, toroidal sliding network puzzle
 * Pattern
 * Pearl, loop-drawing puzzle
 * Pegs, peg solitaire puzzle
 * Range, visible-distance puzzle
 * Rectangles
 * Same Game, block-clearing puzzle
 * Signpost, square-connecting puzzle
 * Singles, number-removing puzzle
 * Sixteen, toroidal sliding block puzzle
 * Slant, maze-drawing puzzle
 * Solo, number placement puzzle
 * Tents, tent-placing puzzle
 * Towers, tower-placing Latin square puzzle
 * Twiddle, rotational sliding block puzzle
 * Undead, monster-placing puzzle
 * Unequal, Latin square puzzle
 * Unruly, black and white grid puzzle
 * Untangle, planar graph layout puzzle")
    (license license:expat)))

sgt-puzzles
