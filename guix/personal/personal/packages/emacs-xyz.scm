;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Divya R. Pattanaik <divya@subvertising.org>
;;; https://codeberg.org/divyaranjan/divya-lambda/src/branch/master/divya-lambda/packages/emacs-xyz.scm
;;;
;;; This file is not part of GNU Guix
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Emacs packages for Guix

(define-module (personal packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses)
                #:prefix license:)

  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pdf))

(define-public emacs-reader
  (package
    (name "emacs-reader")
    (properties '((commit . "40784458ca8ddcc9ded7d350ff71c8d49995e724")))
    (version (git-version "0.3.2" "3" (assoc-ref properties 'commit)))
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://codeberg.org/divyaranjan/emacs-reader")
            (commit (assoc-ref properties 'commit))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0qg1a4cqjhcyvi93gvh93am9y2y77jnnnpmmhcf4i0nshy56yy7q"))))
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


(define-public emacs-auto-minor-mode
  (package
    (name "emacs-auto-minor-mode")
    (version "20180527.1123")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joewreschnig/auto-minor-mode")
             (commit "17cfa1b54800fdef2975c0c0531dad34846a5065")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jgq9b262pjr6npza3k0p2glb6mpp0dfpslgx3i2p8a5ipwhwaqa"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/joewreschnig/auto-minor-mode")
    (synopsis "Enable minor modes by file name and contents")
    (description
     "This package lets you enable minor modes based on file name and contents.  To
find the right modes, it checks filenames against patterns in
‘auto-minor-mode-alist’ and file contents against ‘auto-minor-mode-magic-alist’.
 These work like the built-in Emacs variables ‘auto-mode-alist’ and
‘magic-mode-alist’.  Unlike major modes, all matching minor modes are enabled,
not only the first match.  A reason you might want to use it: (add-to-list
auto-minor-mode-alist (\"-theme\\\\.el\\\\'\" .  rainbow-mode)) There’s intentionally
no equivalent of ‘interpreter-mode-alist’.  Interpreters should determine the
major mode.  Relevant minor modes can then be enabled by major mode hooks.
Minor modes are set whenever ‘set-auto-mode’, the built-in function responsible
for handling automatic major modes, is called.  If you also use ‘use-package’,
two new keywords are added, ‘:minor’ and ‘:magic-minor’, which register entries
in these alists.  You must load (and not defer) ‘auto-minor-mode’ before using
these keywords for other packages.")
    (license #f)))

(define-public emacs-better-jumper
  (package
    (name "emacs-better-jumper")
    (version "20241009.1517")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gilbertw1/better-jumper")
             (commit "b1bf7a3c8cb820d942a0305e0e6412ef369f819c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cq99w9lpd9sg7hb6i9r6qirq626xcgzyjbk438h8qrjgm3xigh4"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/gilbertw1/better-jumper")
    (synopsis "Configurable jump list")
    (description
     "Better-jumper is configurable jump list implementation for Emacs that can be
used to easily jump back to previous locations.  That provides optional
integration with evil.  To enable globally: (require better-jumper)
(better-jumper-mode 1) See included README.md for more information.")
    (license #f)))

(define-public emacs-nerd-icons-corfu
  (package
    (name "emacs-nerd-icons-corfu")
    (version "20250729.1544")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LuigiPiucco/nerd-icons-corfu")
             (commit "f821e953b1a3dc9b381bc53486aabf366bf11cb1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "036p45wqwrqhn5xv5sn3gsm2mb79gj6fk24zpkfa7wrv45qqgb21"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-nerd-icons))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
    (synopsis "Icons for Corfu via nerd-icons")
    (description
     "Introduces a margin formatter for Corfu which adds icons.  The icons are
configurable, but should be text icons provided by the icons fonts in
`nerd-icons'.  To use, install the package and add the following to your init:
(add-to-list corfu-margin-formatters #'nerd-icons-corfu-formatter).")
    (license #f)))

(define-public emacs-yasnippet-capf
  (package
    (name "emacs-yasnippet-capf")
    (version "20250520.1105")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elken/yasnippet-capf")
             (commit "f53c42a996b86fc95b96bdc2deeb58581f48c666")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hwsra5w150dfswkvw3jryhkg538nm3ig74xzfplzbg0n6v7qs19"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-yasnippet))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/elken/yasnippet-capf")
    (synopsis "Yasnippet Completion At Point Function")
    (description
     "Yasnippet Completion at Point Function to lookup snippets by name Simply add to
the list of existing `completion-at-point-functions thus: (add-to-list
completion-at-point-functions #'yasnippet-capf) If you prefer to have the lookup
done by name rather than key, set `yasnippet-capf-lookup-by'.")
    (license #f)))

(define-public emacs-embark-consult
  (package
    (name "emacs-embark-consult")
    (version "20250622.535")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oantolin/embark")
             (commit "7b3b2fa239c34c2e304eab4367a4f5924c047e2b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hn4x5smylqviaqw35j9wccffsvfxp0q5jlnw0ynxny5c7pnp66l"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat emacs-embark emacs-consult))
    (arguments
     '(#:tests? #f #:include '("^embark-consult.el$")
       #:exclude '()))
    (home-page "https://github.com/oantolin/embark")
    (synopsis "Consult integration for Embark")
    (description
     "This package provides integration between Embark and Consult.  The package will
be loaded automatically by Embark.  Some of the functionality here was
previously contained in Embark itself: - Support for consult-buffer, so that you
get the correct actions for each type of entry in consult-buffer's list. -
Support for consult-line, consult-outline, consult-mark and consult-global-mark,
so that the insert and save actions don't include a weird unicode character at
the start of the line, and so you can export from them to an occur buffer (where
occur-edit-mode works!).  Just load this package to get the above functionality,
no further configuration is necessary.  Additionally this package contains some
functionality that has never been in Embark: access to Consult preview from
auto-updating Embark Collect buffer that is associated to an active minibuffer
for a Consult command.  For information on Consult preview, see Consult's info
manual or its readme on @code{GitHub}.  If you always want the minor mode
enabled whenever it possible use: (add-hook embark-collect-mode-hook
#'consult-preview-at-point-mode) If you don't want the minor mode automatically
on and prefer to trigger the consult previews manually use this instead:
(keymap-set embark-collect-mode-map \"C-j\" #'consult-preview-at-point).")
    (license #f)))

(define-public emacs-nerd-icons-completion
  (package
    (name "emacs-nerd-icons-completion")
    (version "20251029.2106")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rainstormstudio/nerd-icons-completion")
             (commit "d09ea987ed3d2cc64137234f27851594050e2b64")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022yfkfvcywgjplvsj5xajmc24q1c7yx0l5mvnzagjfdg4iajidv"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-nerd-icons emacs-compat))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/rainstormstudio/nerd-icons-completion")
    (synopsis "Add icons to completion candidates")
    (description
     "Add nerd-icons to completion candidates.  nerd-icons-completion is inspired by
`all-the-icons-completion': https://github.com/iyefrat/all-the-icons-completion.")
    (license #f)))

(define-public emacs-nav-flash
  (package
    (name "emacs-nav-flash")
    (version "20220726.1117")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rolandwalker/nav-flash")
             (commit "5d4b48567862f6be0ca973d6b1dca90e4815cb9b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l6zamrh3n3416pgr2jhqabldl180zg0n4651g42jn8xcbwg4w6c"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "http://github.com/rolandwalker/nav-flash")
    (synopsis "Briefly highlight the current line")
    (description
     "Quickstart (require nav-flash) (nav-flash-show) Explanation Nav-flash
temporarily highlights the line containing the point, which is sometimes useful
for orientation after a navigation command.  To use nav-flash, place the
nav-flash.el library somewhere Emacs can find it, and add the following to your
~/.emacs file: (require nav-flash) There is no user-level interface for this
library; it is only used by other Lisp libraries.  However, you might find it
useful to call `nav-flash-show in your ~/.emacs file.  For example, the
following hook causes a flash to appear after navigating via imenu: (add-hook
imenu-after-jump-hook nav-flash-show nil t) See Also M-x customize-group RET
nav-flash RET M-x customize-group RET pulse RET Notes This library reuses a
timer and overlay defined in compile.el, but should not affect the normal use of
compile.el / `next-error'.  Pulse.el provides similar functionality and is
included with Emacs.  This library can use pulse.el, but does not do so by
default, because pulse.el uses `sit-for', breaking this type of construction:
(nav-flash-show) (with-temp-message \"message here\" (sit-for 2)) When using an
overlay and timer for cleanup (as nav-flash does by default) the flash and
message appear simultaneously.  Nav-flash.el is also simpler than pulse.el.
Compatibility and Requirements GNU Emacs version 25.1-devel : not tested GNU
Emacs version 24.5 : not tested GNU Emacs version 24.4 : yes GNU Emacs version
24.3 : yes GNU Emacs version 23.3 : yes GNU Emacs version 22.2 : yes, with some
limitations GNU Emacs version 21.x and lower : unknown No external dependencies
Bugs No known bugs.  TODO Check pulse period on other platforms. ; License
Simplified BSD License: Redistribution and use in source and binary forms, with
or without modification, are permitted provided that the following conditions
are met: 1.  Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.  2.
Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.  This software is provided by
Roland Walker \"AS IS\" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability and fitness for a
particular purpose are disclaimed.  In no event shall Roland Walker or
contributors be liable for any direct, indirect, incidental, special, exemplary,
or consequential damages (including, but not limited to, procurement of
substitute goods or services; loss of use, data, or profits; or business
interruption) however caused and on any theory of liability, whether in
contract, strict liability, or tort (including negligence or otherwise) arising
in any way out of the use of this software, even if advised of the possibility
of such damage.  The views and conclusions contained in the software and
documentation are those of the authors and should not be interpreted as
representing official policies, either expressed or implied, of Roland Walker.")
    (license #f)))

(define-public emacs-treemacs-nerd-icons
  (package
    (name "emacs-treemacs-nerd-icons")
    (version "20251024.1914")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rainstormstudio/treemacs-nerd-icons")
             (commit "0c5ddcb978da639f01ddb023febc40fc755171e5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kmgxzskfkv6rz0s60p6pvwsp68c040060i9nnxx1fx5q2zjgzjd"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-nerd-icons emacs-treemacs))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/rainstormstudio/treemacs-nerd-icons")
    (synopsis "Emacs Nerd Font Icons theme for treemacs")
    (description "nerd-icons theme for treemacs.")
    (license #f)))

(define-public emacs-evil-easymotion
  (package
    (name "emacs-evil-easymotion")
    (version "20200424.135")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PythonNut/evil-easymotion")
             (commit "f96c2ed38ddc07908db7c3c11bcd6285a3e8c2e9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xsva9bnlfwfmccm38qh3yvn4jr9za5rxqn4pwxbmhnx4rk47cch"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-avy))
    (inputs (list emacs-evil))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/pythonnut/evil-easymotion")
    (synopsis "A port of vim's easymotion to emacs")
    (description
     "This is a clone of the popular easymotion package for vim, which describes
itself in these terms: > @code{EasyMotion} provides a much simpler way to use
some motions in vim. > It takes the <number> out of <number>w or <number>f{char}
by > highlighting all possible choices and allowing you to press one key > to
jump directly to the target.  If you're having trouble picturing this, please
visit the github repo for a screencast.  Usage/status ============
evil-easymotion, rather unsurprisingly can use evil.  However, you don't _need_
evil to use it.  evil-easymotion can happily define motions for regular emacs
commands.  With that said, evil is recommended, not least because it's awesome.
Currently most motions are supported, and it's easy to define your own
easymotions. (evilem-define (kbd \"SPC w\") evil-forward-word-begin) To define
easymotions for all motions that evil defines by default, add
(evilem-default-keybindings \"SPC\") This binds all motions under the prefix `SPC`
in `evil-motion-state-map`.  This is not done by default for motions defined
manually.  You will need to supply the prefix.  More advanced use-cases are
detailed in the github README.")
    (license #f)))

(define-public emacs-evil-embrace
  (package
    (name "emacs-evil-embrace")
    (version "20230820.445")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cute-jumper/evil-embrace.el")
             (commit "3081d37811b6a3dfaaf01d578c7ab7a746c6064d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13rqkdhhzvnw3s49zm3v9xska8j8l1mr85czcfaf5vrm99lx8rl3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-embrace emacs-evil-surround))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/cute-jumper/evil-embrace.el")
    (synopsis "Evil integration of embrace.el")
    (description
     "______________ EVIL-EMBRACE Junpeng Qiu ______________ Table of Contents
_________________ 1 Overview 2 Why 3 Usage 4 Screencasts Evil integration of
[embrace.el]. [embrace.el] https://github.com/cute-jumper/embrace.el 1 Overview
========== This package provides evil integration of [embrace.el].  Since
`evil-surround provides a similar set of features as `embrace.el', this package
aims at adding the goodies of `embrace.el to `evil-surround and making
`evil-surround even better. [embrace.el]
https://github.com/cute-jumper/embrace.el 2 Why ===== `evil-surround is good
when there is a text object defined.  But unfortunately, if you want to add
custom surrouding pairs, `evil-surround will not be able to delete/change the
pairs if there are no evil text objects defined for these pairs.  For example,
if you want to make `\\textbf{ and `} as a surround pair in `@code{LaTeX-mode}',
you can't either change or delete the surround pair since there is no text
object for `\\textbf{ and `}'.  However, using `embrace', you can define whatever
surrounding pairs you like, and adding, changing, and deleting will *always*
work.  The idea of this package is that let `evil-surround handle the keys that
corresponds to existing text objects (i.e., `(', `[', etc.), which is what
`evil-surround is good at, and make `embrace handles all the other keys of
custom surrounding pairs so that you can also benefit from the extensibility
that `embrace offers.  In a word, you can use the default `evil-surround'.  But
whenever you want to add a custom surrounding pair, use `embrace instead.  To
see how to add a custom pair in `embrace', look at the README of [embrace.el].
[embrace.el] https://github.com/cute-jumper/embrace.el 3 Usage ======= To enable
the `evil-surround integration: ,---- |
(evil-embrace-enable-evil-surround-integration) `---- And use
`evil-embrace-disable-evil-surround-integration to disable whenever you don't
like it.  The keys that are processed by `evil-surround are saved in the
variable `evil-embrace-evil-surround-keys'.  The default value is: ,---- | (?\\(
?\\[ ?\\{ ?\\) ?\\] ?\\} ?\\\" ?\\ ?< ?> ?b ?B ?t ?w ?W ?s ?p) `---- Note that this
variable is buffer-local.  You should change it in the hook: ,---- | (add-hook
@code{LaTeX-mode-hook} | (lambda () | (add-to-list
evil-embrace-evil-surround-keys ?o))) `---- Only these keys saved in the
variable are processed by `evil-surround', and all the other keys will be
processed by `embrace'.  If you find the help message popup annoying, use the
following code to disable it: ,---- | (setq evil-embrace-show-help-p nil) `----
4 Screencasts ============= Use the following settings: ,---- | (add-hook
org-mode-hook embrace-org-mode-hook) |
(evil-embrace-enable-evil-surround-integration) `---- In an org-mode file, we
can change the surrounding pair in the following way (note that this whole
process can't be achieved solely by `evil-surround'):
[./screencasts/evil-embrace.gif].")
    (license #f)))

(define-public emacs-evil-snipe
  (package
    (name "emacs-evil-snipe")
    (version "20250505.508")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hlissner/evil-snipe")
             (commit "16317d7e54313490a0fe8642ed9a1a72498e7ad2")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rg677wdybgjqz8kfr8v7xrcqw53qm1kxcsdsqqq8z0wklb0s29d"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/hlissner/evil-snipe")
    (synopsis "Emulate vim-sneak & vim-seek")
    (description
     "Evil-snipe emulates vim-seek and/or vim-sneak in evil-mode.  It provides
2-character versions of evil's f/F/t/T motions, for quick and more accurately
jumping around text, plus incremental highlighting (for f/F/t/T as well).  To
enable globally: (require evil-snipe) (evil-snipe-mode 1) To replace evil-mode's
f/F/t/T functionality with (1-character) sniping: (evil-snipe-override-mode 1)
See included README.md for more information.")
    (license #f)))

(define-public emacs-exato
  (package
    (name "emacs-exato")
    (version "20200524.1319")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ninrod/exato")
             (commit "aee7af7b7a0e7551478f453d1de7d5b9cb2e06c4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m98bwj8dy90ifck8rsda6zfgbjrv5z0166pp7qzvwls9rqa695m"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/ninrod/exato")
    (synopsis "EXATO: Evil XML/HTML Attributes Text Object")
    (description
     "This package provides the `x` text object to manipulate html/xml tag attributes.
 it is a port of https://github.com/whatyouhide/vim-textobj-xmlattr vim plugin.
Try using `dax`, `vix` and `@code{gUix`}.  You can customize the binding.  To
install the package, Just use https://melpa.org.  Here's an oneliner using
https://github.com/jwiegley/use-package: (use-package exato :ensure t)
*customization*: to change the bind from `x` to your liking, you can customize
exato-key: (use-package exato :ensure t :init (setq exato-key \"h\")).")
    (license #f)))

(define-public emacs-vimish-fold
  (package
    (name "emacs-vimish-fold")
    (version "20251023.1551")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matsievskiysv/vimish-fold")
             (commit "f71f374d28a83e5f15612fa64aac1b2e78be2dcd")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jsfp9kz1ydxck8ds5rghw1aqpmlz0k3l39glzcs8gq0jvb0q8fl"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-f))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/mrkkrp/vimish-fold")
    (synopsis "Fold text like in Vim")
    (description
     "This is a package to perform text folding like in Vim.  It has the following
features: * folding of active regions; * good visual feedback: it's obvious
which part of text is folded; * persistence by default: when you kill a buffer
your folds don't disappear; * persistence scales well, you can work on hundreds
of files with lots of folds without adverse effects; * it does not break
indentation; * folds can be toggled from folded state to unfolded and back very
easily; * quick navigation between existing folds; * you can use mouse to unfold
folds (good for beginners and not only for them); * for fans of `avy package:
you can use `avy to fold text with minimal number of key strokes!")
    (license #f)))

(define-public emacs-evil-vimish-fold
  (package
    (name "emacs-evil-vimish-fold")
    (version "20200122.117")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexmurray/evil-vimish-fold")
             (commit "b6e0e6b91b8cd047e80debef1a536d9d49eef31a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14qhfhk3d4c7v4jhr909dbxy8222flpqwk73bwg0pqwpkcifyv7n"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil emacs-vimish-fold))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/alexmurray/evil-vimish-fold")
    (synopsis "Integrate vimish-fold with evil")
    (description
     "Integrate `vimish-fold with `evil'.  Provides bindings to create and delete
folds via \"zf\" and \"zd\" respectively, and provides integration of usual vim fold
commands via `vimish-fold`.  Also supports navigation between folds using \"zj\" /
\"zk\" respectively.")
    (license #f)))

(define-public emacs-browse-at-remote
  (package
    (name "emacs-browse-at-remote")
    (version "20251223.2328")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rmuslimov/browse-at-remote")
             (commit "cf0269f3db9e968c819b1d85b33d791c20c2e495")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ps67qpcbmr2csgjy9cs0934vv108da1gbs0n219l8visvjjcb34"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-f emacs-s))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/rmuslimov/browse-at-remote")
    (synopsis
     "Open github/gitlab/bitbucket/stash/gist/phab/sourcehut page from Emacs")
    (description
     "Easily open target page on github/gitlab (or bitbucket) from Emacs.  by calling
`browse-at-remote` function.  Support dired buffers and opens them in tree mode
at destination.")
    (license #f)))

(define-public emacs-consult-dash
  (package
    (name "emacs-consult-dash")
    (version "20250114.1511")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/consult-dash-" version
                           ".tar"))
       (sha256
        (base32 "1sb74hcq0k684a2ah4iv250qwpmlyw592hc00iww37c2gmdhjfvl"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash-docs emacs-consult))
    (arguments '(#:tests? #f))
    (home-page "https://codeberg.org/ravi/consult-dash")
    (synopsis "Consult front-end for dash-docs")
    (description
     "consult-dash is the only interface function, a consult front-end for dash-docs.
Embark integration is automatically provided. ; To do - Avoid concatenating
commands through the shell.")
    (license #f)))

(define-public emacs-makefile-executor
  (package
    (name "emacs-makefile-executor")
    (version "20230224.1329")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Olivia5k/makefile-executor.el")
             (commit "d1d98eaf522a767561f6c7cbd8d2526be58b3ec5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wm0i2m124dglwq0szp6pdh2r0dln0xpgscw2immi9cchcmgcy4f"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-f emacs-s))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/Olivia5k/makefile-executor.el")
    (synopsis "Commands for conveniently running makefile targets")
    (description
     "This package provides a set of tools aimed at working with Makefiles on a
project level.  Currently available: - Interactively selecting a make target and
running it.  Bound to C-c C-e when makefile-executor-mode is enabled. -
Re-running the last execution.  We usually run things in Makefiles many times
after all! Bound to `C-c C-c'` in `makefile-mode` when makefile-executor-mode'`
is enabled. - Running a makefile target in a dedicated buffer.  Useful when
starting services and other long-running things! Bound to `C-c C-d'` in
`makefile-mode` when makefile-executor-mode'` is enabled. - Calculation of
variables et.c.; $(BINARY) will show up as what it evaluates to. - Via
`project.el', execution from any buffer in a project.  If more than one makefile
is found, an interactive prompt for one is shown.  If `projectile is installed,
this is added to the `projectile-commander on the m key.  To enable it, use the
following snippet to add the hook into makefile-mode': (add-hook
makefile-mode-hook makefile-executor-mode).")
    (license #f)))

(define-public emacs-gptel-magit
  (package
    (name "emacs-gptel-magit")
    (version "20250520.833")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ragnard/gptel-magit")
             (commit "f27c01821b67ed99ddf705c2b995f78b71394d8b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jsq6jjka0visrm0fdvxd05p78d3n4gkl4i0pc1g825swcfqd182"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-magit emacs-gptel))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/ragnard/gptel-magit")
    (synopsis "Generate commit messages for magit using gptel")
    (description
     "This package uses the gptel library to add LLM integration into magit.
Currently, it adds functionality for generating commit messages.")
    (license #f)))

(define-public emacs-flycheck-popup-tip
  (package
    (name "emacs-flycheck-popup-tip")
    (version "20170812.2351")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flycheck/flycheck-popup-tip")
             (commit "ef86aad907f27ca076859d8d9416f4f7727619c6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bi6f9nm4bylsbjv4qnkar35s6xzdf2cc2cxi3g691p9527apdz6"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck emacs-popup))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/flycheck/flycheck-popup-tip/")
    (synopsis "Display Flycheck error messages using popup.el")
    (description
     "This is extension for Flycheck.  It displays Flycheck error messages in buffer
using `popup.el library.  For more information about Flycheck:
http://www.flycheck.org/ https://github.com/flycheck/flycheck For more
information about this Flycheck extension:
https://github.com/flycheck/flycheck-popup-tip ;; Setup Add to your `init.el':
(with-eval-after-load flycheck (add-hook flycheck-mode-hook
flycheck-popup-tip-mode)).")
    (license #f)))

(define-public emacs-clojure-ts-mode
  (package
    (name "emacs-clojure-ts-mode")
    (version "20251202.1521")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure-emacs/clojure-ts-mode")
             (commit "96fdffcbe9e1b8ebf9ad14e23b06f62cc3422e22")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j78j9ig2x3g8qgsdrs38r3v0rva48c074d7kyag1aa0p7s37kr0"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "http://github.com/clojure-emacs/clojure-ts-mode")
    (synopsis "Major mode for Clojure code")
    (description
     "This package provides font-lock, indentation, and navigation for the Clojure
programming language (http://clojure.org).  For the Tree-sitter grammar this
mode is based on, see https://github.com/sogaiu/tree-sitter-clojure.  Using
clojure-ts-mode with paredit or smartparens is highly recommended.  Here are
some example configurations: ;; require or autoload paredit-mode (add-hook
clojure-ts-mode-hook #'paredit-mode) ;; require or autoload smartparens
(add-hook clojure-ts-mode-hook #'smartparens-strict-mode) See inf-clojure
(http://github.com/clojure-emacs/inf-clojure) for basic interaction with Clojure
subprocesses.  See CIDER (http://github.com/clojure-emacs/cider) for better
interaction with subprocesses via @code{nREPL}.")
    (license #f)))

(define-public emacs-jet
  (package
    (name "emacs-jet")
    (version "20240730.1228")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ericdallo/jet.el")
             (commit "c9a92675efd802f37df5e3eab7858dbbeced6ea4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vza4qwbvj8cz5jsjpz5ysvbk782zsgimxfqyz3h4pygwaxisxqj"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-transient))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/ericdallo/jet.el")
    (synopsis "Emacs integration for jet Clojure tool")
    (description
     "Emacs integration for jet Clojure tool: https://github.com/borkdude/jet The main
entrypoint is the `jet command which will use `transient to display a smooth
interface to customize the command to be executed.  The other public functions
are available to be used as quick commands or to keybind.")
    (license #f)))

(define-public emacs-highlight-quoted
  (package
    (name "emacs-highlight-quoted")
    (version "20140916.1822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/highlight-quoted")
             (commit "24103478158cd19fbcfb4339a3f1fa1f054f1469")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gq8inxfni9zgz2brqm4nlswgr8b0spq15wr532xfrgr456g10ks"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/Fanael/highlight-quoted")
    (synopsis "Highlight Lisp quotes and quoted symbols")
    (description
     "Minor mode proving highlight of Lisp quotes and quoted symbols.")
    (license #f)))

(define-public emacs-overseer
  (package
    (name "emacs-overseer")
    (version "20240109.800")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonini/overseer.el")
             (commit "7fdcf1a6fba6b1569a09c1666b4e51bcde266ed9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f0nm253n0k2rcx0zydj8c4nn5gmvhabzraajxdqycb2ak77nbif"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-pkg-info emacs-f))
    (arguments '(#:tests? #f))
    (home-page "http://www.github.com/tonini/overseer.el")
    (synopsis "Ert-runner Integration Into Emacs")
    (description "Ert-runner Integration Into Emacs.")
    (license #f)))

(define-public emacs-markdown-toc
  (package
    (name "emacs-markdown-toc")
    (version "20251210.2018")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ardumont/markdown-toc")
             (commit "29e5c0f33ed026a5f993e4211f52debd7c02b3ba")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12p9i6sah599lzpki4276g0lla137klnq796n11wkr0cas1rgbyg"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-markdown-mode emacs-dash emacs-s))
    (arguments '(#:tests? #f))
    (home-page "http://github.com/ardumont/markdown-toc")
    (synopsis "A simple TOC generator for markdown file")
    (description
     "Generate a TOC from a markdown file: M-x markdown-toc-generate-toc This will
compute the TOC at insert it at current position.  Update existing TOC: C-u M-x
markdown-toc-generate-toc Here is a possible output: <!-- markdown-toc start -
Don't edit this section.  Run M-x markdown-toc-refresh-toc --> **Table of
Contents** - [some markdown page title](#some-markdown-page-title) - [main
title](#main-title) - [Sources](#sources) - [Marmalade
(recommended)](#marmalade-recommended) - [Melpa-stable](#melpa-stable) - [Melpa
(~snapshot)](#melpa-~snapshot) - [Install](#install) - [Load
org-trello](#load-org-trello) - [Alternative](#alternative) - [Git](#git) -
[Tar](#tar) - [another title](#another-title) - [with](#with) - [some](#some) -
[heading](#heading) <!-- markdown-toc end --> Install - M-x package-install RET
markdown-toc RET.")
    (license #f)))

(define-public emacs-ox-clip
  (package
    (name "emacs-ox-clip")
    (version "20240310.1513")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jkitchin/ox-clip")
             (commit "a549cc8e1747beb6b7e567ffac27e31ba45cb8e8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i94p0nzhx1h181z6whkc3gbja85qk97xvmhx3p03a7b1pjswrhn"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-org emacs-htmlize))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/jkitchin/ox-clip")
    (synopsis "Cross-platform formatted copying for org-mode")
    (description
     "This module copies selected regions in org-mode as formatted text on the
clipboard that can be pasted into other applications.  When not in org-mode, the
htmlize library is used instead.  For Windows the html-clip-w32.py script will
be installed.  It works pretty well, but I noticed that the hyperlinks in the
TOC to headings don't work, and strike-through doesn't seem to work.  I have no
idea how to fix either issue.  Mac OSX needs textutils and pbcopy, which should
be part of the base install.  Linux needs a relatively modern xclip, preferrably
a version of at least 0.12.  https://github.com/astrand/xclip The main command
is `ox-clip-formatted-copy that should work across Windows, Mac and Linux.  By
default, it copies as html.  Note: Images/equations may not copy well in html.
Use `ox-clip-image-to-clipboard to copy the image or latex equation at point to
the clipboard as an image.  The default latex scale is too small for me, so the
default size for this is set to 3 in `ox-clip-default-latex-scale'.  This
overrides the settings in `org-format-latex-options'.")
    (license #f)))

(define-public emacs-gorepl-mode
  (package
    (name "emacs-gorepl-mode")
    (version "20170905.945")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/manute/gorepl-mode")
             (commit "6a73bf352e8d893f89cad36c958c4db2b5e35e07")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ydiq55ylm8ph2r5nlv9p7a5bnnxk3c9731l7mbzdhd43f734dld"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-s emacs-f emacs-hydra))
    (arguments '(#:tests? #f))
    (home-page "http://www.github.com/manute/gorepl-mode")
    (synopsis "Go REPL Interactive Development in top of Gore")
    (description
     "This library provides a Go repl interactive development environment for Emacs,
built on top of Gore (https://github.com/motemen/gore).")
    (license #f)))

(define-public emacs-go-tag
  (package
    (name "emacs-go-tag")
    (version "20230111.651")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brantou/emacs-go-tag")
             (commit "33f2059551d5298ca228d90f525b99d1a8d70364")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nmxw99xqhr9sg5lafqjs7x033br8xz9106zc96gxf07v6zgbxy2"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-go-mode))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/brantou/emacs-go-tag")
    (synopsis "Edit Golang struct field tag")
    (description
     "Edit field tags for golang struct fields, based on gomodifytags.  This package
is inspired by @code{GoAddTags} of vim-go and go-add-tags.")
    (license #f)))

(define-public emacs-go-gen-test
  (package
    (name "emacs-go-gen-test")
    (version "20230616.2053")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/s-kostyaev/go-gen-test")
             (commit "af00a9abbaba2068502327ecdef574fd894a884b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q81zkyrl1njwxq29rx7gq9m9w3jags6akxzl7jd9yrnl4k2l27p"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f #:include '("^go-gen-test.el$")
       #:exclude '()))
    (home-page "https://github.com/s-kostyaev/go-gen-test")
    (synopsis "Generate tests for go code with gotests")
    (description
     "This package is simple wrapper for https://github.com/cweill/gotests You should
install `gotests for use it.")
    (license #f)))

(define-public emacs-flycheck-golangci-lint
  (package
    (name "emacs-flycheck-golangci-lint")
    (version "20251203.2053")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/weijiangan/flycheck-golangci-lint")
             (commit "f7e36e19d6af39d098b94a2e7524dbd7b585ce67")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h77vyrx0cswwmqww0ac75vfw9v8ylxfr715rfh3c30920gb2ip8"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/weijiangan/flycheck-golangci-lint")
    (synopsis "Flycheck checker for golangci-lint")
    (description
     "Flycheck checker for golangci-lint Usage: (eval-after-load flycheck (add-hook
flycheck-mode-hook #'flycheck-golangci-lint-setup)).")
    (license #f)))

(define-public emacs-restclient-jq
  (package
    (name "emacs-restclient-jq")
    (version "20250803.2119")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacsorphanage/restclient")
             (commit "1800a4e367c250051617d0b8c16a7cbd7f47da69")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02yphcli11j0p6144rwh7l5whx4ahxm3y15nz0b7r3y04fm25w6g"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-restclient emacs-jq-mode))
    (arguments
     '(#:tests? #f #:include '("^restclient-jq.el$")
       #:exclude '()))
    (home-page "https://github.com/pashky/restclient.el")
    (synopsis "Support for setting restclient vars from jq expressions")
    (description
     "This is a companion to restclient.el to add support for setting variables from
results using jq expressions.")
    (license #f)))

(define-public emacs-flycheck-plantuml
  (package
    (name "emacs-flycheck-plantuml")
    (version "20171018.111")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alexmurray/flycheck-plantuml")
             (commit "183be89e1dbba0b38237dd198dff600e0790309d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fbdbpwrlkvbgv693ndr3zamkf3gp28v94jg911fsav8bk08f6pq"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-flycheck emacs-plantuml-mode))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/alexmurray/flycheck-plantuml")
    (synopsis "Integrate plantuml with flycheck")
    (description
     "This packages integrates plantuml with flycheck to automatically check the
syntax of your plantuml files on the fly ;; Setup (with-eval-after-load flycheck
(require flycheck-plantuml) (flycheck-plantuml-setup)).")
    (license #f)))

(define-public emacs-pip-requirements
  (package
    (name "emacs-pip-requirements")
    (version "20240621.2151")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Wilfred/pip-requirements.el")
             (commit "31e0dc62abb2d88fa765e0ea88b919d756cc0e4f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08q225h8kahh632qkzpb1ih3jqg5imlzgrrh8ynkyxrr710madkl"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/Wilfred/pip-requirements.el")
    (synopsis "A major mode for editing pip requirements files")
    (description
     "This is a major mode for editing pip requirements files, with the following
features: * Syntax highlighting * Togglable comments * Auto completion of
package names from @code{PyPI} TODO: Steal shamelessly all the fantasic ideas in
https://github.com/wuub/requirementstxt.")
    (license #f)))

(define-public emacs-pipenv
  (package
    (name "emacs-pipenv")
    (version "20220514.123")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pwalsh/pipenv.el")
             (commit "3af159749824c03f59176aff7f66ddd6a5785a10")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ak9dvjqhdm12i7yamgbqjmc4zmvy2f0gd1nia1q9dy3n6576ryq"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-s emacs-pyvenv emacs-load-env-vars))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/pwalsh/pipenv.el")
    (synopsis "A Pipenv porcelain")
    (description "See https://github.com/pwalsh/pipenv.el for documentation.")
    (license #f)))

(define-public emacs-python-pytest
  (package
    (name "emacs-python-pytest")
    (version "20250726.1726")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wbolster/emacs-python-pytest")
             (commit "ed2ecee09d1cccb4245842860d91940cb2fda769")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1787bks1zi47qglib42vnlqa7m4899n5vh1ics0013ldd89jqrr1"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-transient emacs-s))
    (arguments '(#:tests? #f #:exclude '()))
    (home-page "https://github.com/wbolster/emacs-python-pytest")
    (synopsis "Helpers to run pytest")
    (description
     "This package provides helpers to run pytest.  See README for details.")
    (license #f)))

(define-public emacs-haml-mode
  (package
    (name "emacs-haml-mode")
    (version "20250714.1441")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nex3/haml-mode")
             (commit "3bb4a96535eb5c81dbe6a43bfa8d67a778d449c0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1svnggkqi7kk2pspgqb6ciqkiypg09gvph9q48mili17xfx44ll7"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/nex3/haml-mode")
    (synopsis "Major mode for editing Haml files")
    (description
     "Because Haml's indentation schema is similar to that of YAML and Python, many
indentation-related functions are similar to those in yaml-mode and python-mode.
 To install, save this on your load path and add the following to your .emacs
file: (require haml-mode).")
    (license #f)))

(define-public emacs-slim-mode
  (package
    (name "emacs-slim-mode")
    (version "20240513.2118")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slim-template/emacs-slim")
             (commit "0b1b3803290f749cb85084adc75013254b513d41")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gzxfbz22mxp5adfyasaspjq8k3fwrpcbgywwvyh4h3c72j9x1a7"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "http://github.com/slim-template/emacs-slim")
    (synopsis "Major mode for editing Slim files")
    (description
     "Because Slim's indentation schema is similar to that of YAML and Python, many
indentation-related functions are similar to those in yaml-mode and python-mode.
 To install, save this on your load path and add the following to your .emacs
file: (require slim-mode).")
    (license #f)))

(define-public emacs-sass-mode
  (package
    (name "emacs-sass-mode")
    (version "20190502.53")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nex3/sass-mode")
             (commit "247a0d4b509f10b28e4687cd8763492bca03599b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nhk12lhvkwdk8s8fx33p6rssi0gcfx2zkanq23rz6k28v5zi5yp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-haml-mode))
    (arguments '(#:tests? #f))
    (home-page "http://github.com/nex3/haml/tree/master")
    (synopsis "Major mode for editing Sass files")
    (description
     "Because Sass's indentation schema is similar to that of YAML and Python, many
indentation-related functions are similar to those in yaml-mode and python-mode.
 To install, save this on your load path and add the following to your .emacs
file: (require sass-mode) sass-mode requires haml-mode, which can be found at
http://github.com/nex3/haml-mode.")
    (license #f)))

(define-public emacs-stylus-mode
  (package
    (name "emacs-stylus-mode")
    (version "20211019.2113")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianc/jade-mode")
             (commit "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n26jlvf0z7h5yq3w2pzznj43g5xknq1icg68pc0ysqdfm4nq51m"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f #:include '("^stylus-mode.el$")
       #:exclude '()))
    (home-page "https://github.com/brianc/jade-mode")
    (synopsis "Major mode for editing .styl files")
    (description
     "Major mode for the Stylus templating language (https://stylus-lang.com/).")
    (license #f)))

(define-public emacs-sws-mode
  (package
    (name "emacs-sws-mode")
    (version "20210908.2121")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianc/jade-mode")
             (commit "1ad7c51f3c6a6ae64550d9510c5e4e8470014375")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n26jlvf0z7h5yq3w2pzznj43g5xknq1icg68pc0ysqdfm4nq51m"))))
    (build-system emacs-build-system)
    (arguments
     '(#:tests? #f #:include '("^sws-mode.el$")
       #:exclude '()))
    (home-page "https://github.com/brianc/jade-mode")
    (synopsis "(S)ignificant (W)hite(S)pace mode")
    (description "Common code for the jade-mode and stylus-mode.")
    (license #f)))

(define-public emacs-torrent-mode
  (package
    (name "emacs-torrent-mode")
    (version "20250813.1529")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sarg/torrent-mode.el")
             (commit "5dbb59c60c0c2db24d3d138eb003c66c3578b7b4")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r6hi18mjlip002lqfbch7fnkkzn5g1h2p3y9c6qmw79kd32qlf8"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-tablist emacs-bencoding))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/sarg/torrent-mode.el")
    (synopsis "Display torrent files in a tabulated view")
    (description "This package displays torrent files using tablist-mode.")
    (license #f)))

(define-public emacs-aria2
  (package
    (name "emacs-aria2")
    (version "20230314.2131")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ukaszg/aria2")
             (commit "1f2cbe624f3a4e0109b5dc123bb4bbed496b15a7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "166l6x802zz32zh6xlblfssd2rpvkkg8lf5apz76dbr4h0m2zw1k"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://bitbucket.org/ukaszg/aria2-mode")
    (synopsis "Control aria2c commandline tool from Emacs")
    (description
     "This package lacks a description.  Run \"info '(guix) Synopses and Descriptions'\" for more information.")
    (license #f)))

(define-public emacs-circe-notifications
  (package
    (name "emacs-circe-notifications")
    (version "20180102.2318")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eqyiel/circe-notifications")
             (commit "291149ac12877bbd062da993479d3533a26862b0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18mva5nn919c86sgk6kdh437vdnlh9bk7fg10xqcpics1yv3viaw"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-circe emacs-alert))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/eqyiel/circe-notifications")
    (synopsis "Add desktop notifications to Circe")
    (description "No description available.")
    (license #f)))

(define-public emacs-iwd-manager
  (package
    (name "emacs-iwd-manager")
    (version "20260103.2138")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sarg/wifi-manager")
             (commit "22b424e6e6fa0eb4353394c64f31657ded09fe2b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12m89lyz0rydx13ndkahg1phkp2afg1wrif52hfs0kn4p7b8yjbx"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-promise))
    (arguments
     '(#:tests? #f #:include '("^iwd-manager.el$")
       #:exclude '()))
    (home-page "https://github.com/sarg/wpa-manager.el")
    (synopsis "Manage IWD via the D-Bus interface")
    (description
     "This package provides a dbus-based client for @code{iNet} Wireless Daemon.
Supports connecting to PSK networks.")
    (license #f)))

(define-public emacs-emms-player-spotify
  (package
    (name "emacs-emms-player-spotify")
    (version "20260109.1935")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sarg/emms-spotify")
             (commit "91fe535d3420fa57c24ef164bf4b47085ed3cd6a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xhb4n7b57jb0m3jq2svqdm3hfraisqlq0i0lli18d5hx4p1jy6j"))))
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

(define-public emacs-bufler
  (package
    (name "emacs-bufler")
    (version "20250327.2246")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/alphapapa/bufler.el")
              (commit "b96822d2132fda6bd1dd86f017d7e76e3b990c82")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbmv6i3yn4kw0bc404nrb3lm72z89cfvdxf18j1sgsj56ykiis0"))
       (snippet #~(begin (delete-file "helm-bufler.el")))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-burly
                             emacs-dash
                             emacs-f
                             emacs-pretty-hydra
                             emacs-magit
                             emacs-map))
    (arguments
     '(#:tests? #f
       #:include '("^[^/]+.el$" "^[^/]+.el.in$"
                   "^dir$"
                   "^[^/]+.info$"
                   "^[^/]+.texi$"
                   "^[^/]+.texinfo$"
                   "^doc/dir$"
                   "^doc/[^/]+.info$"
                   "^doc/[^/]+.texi$"
                   "^doc/[^/]+.texinfo$")
       #:exclude '("^.dir-locals.el$" "^test.el$" "^tests.el$"
                   "^[^/]+-test.el$" "^[^/]+-tests.el$" "^helm-bufler.el$")))
    (home-page "https://github.com/alphapapa/bufler.el")
    (synopsis "Group buffers into workspaces with programmable rules")
    (description
     "Bufler is like a butler for your buffers, presenting them to you in an organized
way based on your instructions.  The instructions are written as grouping rules
in a simple language, allowing you to customize the way buffers are grouped.
The default rules are designed to be generally useful, so you don't have to
write your own.  It also provides a workspace mode which allows frames to focus
on buffers in certain groups.  Since the groups are created automatically, the
workspaces are created dynamically, rather than requiring you to put buffers in
workspaces manually.")
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

(define-public emacs-just-mode
  (package
    (name "emacs-just-mode")
    (version "20251121.1826")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leon-barrett/just-mode.el")
             (commit "b6173c7bf4d8d28e0dbd80fa41b9c75626885b4e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1czf779akdcx72ma7x9v70kjbic73312fi1czbzvlvxr01pjpyj0"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/leon-barrett/just-mode.el")
    (synopsis "Justfile editing mode")
    (description
     "This package provides a major mode for editing justfiles, as defined by the tool
\"just\": https://github.com/casey/just.")
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
    (version "20250301.833")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SqrtMinusOne/eshell-atuin")
             (commit "1ac4895529546839985c7f57c9858644f7be1e6a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zf62qdmqw7y7s1dg3d35abr9jaymyqfbrv4bplkrry2wwk0m4gx"))))
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
    (license #f)))

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

(define-public emacs-acp
  (package
    (name "emacs-acp")
    (version "20251219.2135")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/acp.el")
             (commit "7b67facc657a7388a53ea8bba5d6e7eba20fa3e0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0znm5qihx2qy3hgw0idg8j7bnhz8k3yaadff3y6696qckdh0qlnr"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/xenodium/acp.el")
    (synopsis "An ACP (Agent Client Protocol) implementation")
    (description
     "acp.el implements ACP (Agent Client Protocol) as per spec
https://agentclientprotocol.com Note: This package is in the very early stage
and is likely incomplete or may have some rough edges.  Report issues at
https://github.com/xenodium/acp.el/issues ✨ Please support this work
https://github.com/sponsors/xenodium ✨.")
    (license #f)))

(define-public emacs-agent-shell
  (package
    (name "emacs-agent-shell")
    (version "20260104.1700")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xenodium/agent-shell")
             (commit "9a7913a7c7b9b8e076b38e7133122dc608e7667e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bwxwn8jyzff5ls54iijm18xnahf9sjdn7hw0smfwbr2cldryc5y"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-shell-maker emacs-acp))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/xenodium/agent-shell")
    (synopsis "Native agentic integrations for Claude Code, Gemini CLI, etc")
    (description
     "`agent-shell offers a native `comint shell experience to interact with any agent
powered by ACP (Agent Client Protocol). `agent-shell currently provides access
to Claude Code, Cursor, Gemini CLI, Goose, Codex, @code{OpenCode}, and Qwen
amongst other agents.  This package depends on the `acp package to provide the
ACP layer as per https://agentclientprotocol.com spec.  Report issues at
https://github.com/xenodium/agent-shell/issues ✨ Support this work
https://github.com/sponsors/xenodium ✨.")
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

(define-public emacs-khalel
  (package
    (name "emacs-khalel")
    (version "20250910.946")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/hperrey/khalel")
             (commit "f7cdb3246d193a518b3a4ca7381ffb6ed8087fcf")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06h5272kmg0ykf0zqdy2qwhlzszqsw176l1brk04bg8xyc3a4384"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://gitlab.com/hperrey/khalel")
    (synopsis "Import, edit and create calendar events through khal")
    (description
     "Khalel provides helper routines to import current events from a local calendar
through the command-line tool khal into an org-mode file.  Commands to edit and
to capture new events allow modifications to the calendar.  Changes to the local
calendar can be transfered to remote @code{CalDAV} servers using the
command-line tool vdirsyncer which can be called from within khalel.  First
steps/quick start: - install, configure and run vdirsyncer - install and
configure khal - customize the values for capture file and import file for
khalel - call `khalel-add-capture-template to set up a capture template - import
events through `khalel-import-events', edit them through
`khalel-edit-calendar-event or create new ones through `org-capture - consider
adding the import org file to your org agenda to show current events there.")
    (license #f)))

(define-public emacs-embrace
  (package
    (name "emacs-embrace")
    (version "20231027.419")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cute-jumper/embrace.el")
             (commit "c7e748603151d7d91c237fd2d9cdf56e9f3b1ea8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c6fbkw1hl9bhdy62g782js8i9kgjr0pr132mpga12jd4cwf8mmz"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-expand-region))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/cute-jumper/embrace.el")
    (synopsis "Add/Change/Delete pairs based on `expand-region'")
    (description
     "_____________ EMBRACE.EL Junpeng Qiu _____________ Table of Contents
_________________ 1 Overview 2 Usage ..  2.1 Example ..  2.2 Screencasts ..  2.3
`embrace-change and `embrace-delete ..  2.4 `embrace-add 3 Customization ..  3.1
Adding More Semantic Units ..  3.2 Adding More Surrounding Pairs ..  3.3 Disable
Help Message ..  3.4 Example Settings 4 For `evil-surround Users ..  4.1 Where
`embrace is better ..  4.2 Why not use together? 5 Contributions 6 Related
Packages Add/Change/Delete pairs based on [expand-region].  For `evil-surround
integration, see [evil-embrace]. [expand-region]
https://github.com/magnars/expand-region.el [evil-embrace]
https://github.com/cute-jumper/evil-embrace.el 1 Overview ========== This
package is heavily inspired by [evil-surround] (which is a port of the vim
plugin [surround.vim]).  But instead of using `evil and its text objects, this
package relies on another excellent package [expand-region].  For Emacs users
who don't like `evil and thus don't use `evil-surround', `embrace provides
similar commands that can be found in `evil-surround'. `Evil is absolutely *not*
required.  For `evil-surround users, `embrace can make your `evil-surround
commands even better! (Have you noticed that `evil-surround doesn't work on many
custom pairs?) [evil-surround] https://github.com/timcharper/evil-surround
[surround.vim] https://github.com/tpope/vim-surround [expand-region]
https://github.com/magnars/expand-region.el 2 Usage ======= There are three
commands: `embrace-add', `embrace-change and `embrace-delete that can add,
change, and delete surrounding pairs respectively.  You can bind these commands
to your favorite key bindings.  There is also a dispatch command
`embrace-commander'.  After invoking `embrace-commander', you can hit: - `a for
`embrace-add - `c for `embrace-change - `d for `embrace-delete 2.1 Example
~~~~~~~~~~~ It might be a little hard for users who have no experience in `evil
and `evil-surround to understand what `embrace can do.  So let's give an example
to show what `embrace can do fist.  You can look at the following sections to
see the meaning of key bindings.  In this example, I bind C-, to
`embrace-commander'.  Assume we have following text in `c-mode and the cursor
position is indicated by `|': ,---- | fo|o `---- Press C-, a w  to add  to the
current word: ,---- | fo|o `---- Press C-, a q { to add {} to outside of the
quotes: ,---- | {'fo|o'} `---- Press C-, c  \" to change the  to \"\": ,---- |
{\"fo|o\"} `---- Press C-, c { t, and then enter the tag: body class=\"page-body\",
to change the {} to a tag: ,---- | <body class=\"page-body\">\"fo|o\"</body> `----
Press C-, c t f, and enter the function name `bar to change the tag to a
function call: ,---- | bar(\"fo|o\") `---- Press C-, d f to remove the function
call: ,---- | \"fo|o\" `---- If you're an `evil-surround user, you might notice
that the last command can't be achieved by `evil-surround'.  However, it works
in `embrace'! And yes, you can find even more examples in which `evil-surround
doesn't work while `embrace works! 2.2 Screencasts ~~~~~~~~~~~~~~~ For non
`evil-mode users, use the following settings (they will be explained later):
,---- | (global-set-key (kbd \"C-,\") #'embrace-commander) | (add-hook
org-mode-hook #'embrace-org-mode-hook) `---- Open an org-mode file, we can
perform the following pair changing: [./screencasts/embrace.gif] For `evil-mode
users, here is a similar screencast (see [evil-embrace] for more details):
[https://github.com/cute-jumper/evil-embrace.el/blob/master/screencasts/evil-embrace.gif]
[evil-embrace] https://github.com/cute-jumper/evil-embrace.el 2.3
`embrace-change and `embrace-delete ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These two commands can change and delete the surround pair respectively.  For
`evil-surround users, `embrace-change is similar to `cs and `embrace-delete is
similar to `ds'.  The surrounding pair is specified by a key, which is very
similar to the key used for Vim's text objects.  For example, `( stands for the
surrounding pair `( and `)', and `{ stands for the surrouding pair, `{ and `}'.
The default key mappings are shown below: Key Left right
-------------------------------- ( \"(\" \")\" ) \"( \" \" )\" { \"{\" \"}\" } \"{ \" \" }\" [
\"[\" \"]\" ] \"[ \" \" ]\" > \"<\" \">\" \" \"\\\"\" \"\\\"\"  \"\\'\" \"\\'\" ` \"`\" \"`\" t \"<foo bar=100>\"
\"</foo>\" f \"func(\" \")\" Note that for `t and `f key, the real content is based on
the user's input.  Also, you can override the closing quote when entering a `
(backquote) in emacs-lisp to get a  (apostrophe) instead of a ` (backquote) by
using `embrace-emacs-lisp-mode-hook (see below).  2.4 `embrace-add
~~~~~~~~~~~~~~~~~ This command is similar to `evil-surround''s `ys command.  We
need to enter a key for the semantic unit to which we want to add a surrounding
pair.  The semantic unit is marked by the functions provided by `expand-region'.
 Here is the default mapping: key mark function ----------------------------- w
er/mark-word s er/mark-symbol d er/mark-defun p er/mark-outside-pairs P
er/mark-inside-pairs q er/mark-outside-quotes Q er/mark-inside-quotes .
er/mark-sentence h er/mark-paragraph After pressing a key to select the semantic
unit, you can press another key to add the surrounding pair, which is the same
as `embrace-change and `embrace-delete'.  3 Customization =============== 3.1
Adding More Semantic Units ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ You can modify the
variable `embrace-semantic-units-alist and note that this variable is
buffer-local so it is better to change the value in a hook: ,---- | (add-hook
text-mode-hook | (lambda () | (add-to-list embrace-semantic-units-alist (?e .
er/mark-email)))) `---- 3.2 Adding More Surrounding Pairs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Use the command `embrace-add-pair to add a
pair: ,---- | (embrace-add-pair key left right) `---- The change is also
buffer-local, so wrap it in a hook function: ,---- | (add-hook
@code{LaTeX-mode-hook} | (lambda () | (embrace-add-pair ?e \"\\\\begin{\" \"}\")))
`---- If you want add something like the `t key for the tag, you can look at the
function `embrace-add-pair-regexp in the source code.  Note that if you're using
`embrace-add-pair to add an existing key, then it will replace the old one.  3.3
Disable Help Message ~~~~~~~~~~~~~~~~~~~~~~~~ If you find the help message
annoying, use the following code to disable it: ,---- | (setq
embrace-show-help-p nil) `---- 3.4 Example Settings ~~~~~~~~~~~~~~~~~~~~ I
recommend binding a convenient key for `embrace-commander'.  For example, ,----
| (global-set-key (kbd \"C-,\") #'embrace-commander) `---- We have defined several
example hook functions that provide additional key bindings which can be used in
different major modes.  Right now there are hooks for `@code{LaTeX-mode}',
`org-mode', `ruby-mode (including `enh-ruby-mode') and `emacs-lisp-mode':
`@code{LaTeX-mode}': Key Left Right ---------------------- = \\verb| | ~ \\texttt{
} * \\textbf{ } `org-mode': Key Left Right
------------------------------------------ = = = ~ ~ ~ * * * _ _ _ + + + k
`@@@@html:<kbd>@@@@ `@@@@html:</kbd>@@@@ `ruby-mode and `enh-ruby-mode': Key
Left Right ------------------ # #{ } d do end `emacs-lisp-mode': Key Left Right
------------------ ` `  To use them: ,---- | (add-hook @code{LaTeX-mode-hook}
embrace-@code{LaTeX-mode-hook}) | (add-hook org-mode-hook embrace-org-mode-hook)
| (add-hook ruby-mode-hook embrace-ruby-mode-hook) ;; or enh-ruby-mode-hook |
(add-hook emacs-lisp-mode-hook embrace-emacs-lisp-mode-hook) `---- The code of
two of the hooks above (which are defined in `embrace.el'): ,---- | (defun
embrace-@code{LaTeX-mode-hook} () | (dolist (lst ((?= \"\\\\verb|\" . \"|\") | (?~
\"\\\\texttt{\" . \"}\") | (?/ \"\\\\emph{\" . \"}\") | (?* \"\\\\textbf{\" . \"}\"))) |
(embrace-add-pair (car lst) (cadr lst) (cddr lst)))) | (defun
embrace-org-mode-hook () | (dolist (lst ((?= \"=\" . \"=\") | (?~ \"~\" . \"~\") | (?/
\"/\" . \"/\") | (?* \"*\" . \"*\") | (?_ \"_\" . \"_\") | (?+ \"+\" . \"+\") | (?k
\"@@@@html:<kbd>@@@@\" . \"@@@@html:</kbd>@@@@\"))) | (embrace-add-pair (car lst)
(cadr lst) (cddr lst)))) `---- You can define and use your own hook function
similar to the code above.  Welcome to add some settings for more major modes.
4 For `evil-surround Users =========================== 4.1 Where `embrace is
better ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ From the previous example, you can see that
`embrace actually replicates all the funcionalities provided in `evil-surround
and it can even do more than `evil-surround'.  Actually, they are quite
different.  Since `embrace uses `expand-region behind the scene, you can expect
it to work as long as `expand-region works.  Unlike `evil-surround', which is
restricted to the pre-defined text objects, `embrace can define nearly arbitrary
surrounding pairs and three core commands always work.  On the contratry, you
get nearly no customization in `evil-surround': custom pairs don't work in `cs
or `ds if you don't have a corresponding text object defined (they work in
`ys'). *TL;DR*: `embrace is more customizable.  4.2 Why not use together?
~~~~~~~~~~~~~~~~~~~~~~~~~ Sure! You can make `embrace and `evil-surround work
together.  Look at [evil-embrace]! [evil-embrace]
https://github.com/cute-jumper/evil-embrace.el 5 Contributions ===============
This package is still in early stage, but it is quite usable right now.  More
functions can be added and the evil integration is not perfect yet.
Contributions are always welcome! 6 Related Packages ================== -
[evil-embrace] - [expand-region] - [evil-surround] - [change-inner] -
[smartparens] [evil-embrace] https://github.com/cute-jumper/evil-embrace.el
[expand-region] https://github.com/magnars/expand-region.el [evil-surround]
https://github.com/timcharper/evil-surround [change-inner]
https://github.com/magnars/change-inner.el [smartparens]
https://github.com/Fuco1/smartparens.")
    (license #f)))

(define-public emacs-expand-region
  (package
    (name "emacs-expand-region")
    (version "20241217.1840")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/magnars/expand-region.el")
             (commit "351279272330cae6cecea941b0033a8dd8bcc4e8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d6lvds7wfp9xsx5mh4x4sgync295r0bw0akmv136j5ks56xigf1"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/magnars/expand-region.el")
    (synopsis "Increase selected region by semantic units")
    (description
     "Expand region increases the selected region by semantic units.  Just keep
pressing the key until it selects what you want.  An example: (setq
alphabet-start \"abc def\") With the cursor at the `c`, it starts by marking the
entire word `abc`, then expand to the contents of the quotes `abc def`, then to
the entire quote `\"abc def\"`, then to the contents of the sexp `setq
alphabet-start \"abc def\"` and finally to the entire sexp.  You can set it up
like this: (require expand-region) (global-set-key (kbd \"C-=\") er/expand-region)
There's also `er/contract-region` if you expand too far. ## Video You can [watch
an intro to expand-region at Emacs Rocks](http://emacsrocks.com/e09.html). ##
Language support Expand region works fairly well with most languages, due to the
general nature of the basic expansions: er/mark-word er/mark-symbol
er/mark-method-call er/mark-inside-quotes er/mark-outside-quotes
er/mark-inside-pairs er/mark-outside-pairs However, most languages also will
benefit from some specially crafted expansions.  For instance, expand-region
comes with these extra expansions for html-mode: er/mark-html-attribute
er/mark-inner-tag er/mark-outer-tag You can add your own expansions to the
languages of your choice simply by creating a function that looks around point
to see if it's inside or looking at the construct you want to mark, and if so -
mark it.  There's plenty of examples to look at in these files.  After you make
your function, add it to a buffer-local version of the `er/try-expand-list`.
**Example:** Let's say you want expand-region to also mark paragraphs and pages
in text-mode.  Incidentally Emacs already comes with `mark-paragraph` and
`mark-page`.  To add it to the try-list, do this: (defun
er/add-text-mode-expansions () (setq-local er/try-expand-list (append
er/try-expand-list (mark-paragraph mark-page)))) (er/enable-mode-expansions
text-mode #'er/add-text-mode-expansions) Add that to its own file, and require
it at the bottom of this one, where it says \"Mode-specific expansions\"
**Warning:** Badly written expansions might slow down expand-region
dramatically.  Remember to exit quickly before you start traversing the entire
document looking for constructs to mark. ## Contribute If you make some nice
expansions for your favorite mode, it would be great if you opened a
pull-request.  The repo is at: https://github.com/magnars/expand-region.el
Changes to `expand-region-core` itself must be accompanied by feature tests.
They are written in [Ecukes](http://ecukes.info), a Cucumber for Emacs.  To
fetch the test dependencies: $ cd /path/to/expand-region $ git submodule init $
git submodule update Run the tests with: $ ./util/ecukes/ecukes features If you
want to add feature-tests for your mode-specific expansions as well, that is
utterly excellent. ## Contributors * [Josh Johnston](https://github.com/joshwnj)
contributed `er/contract-region` * [Le Wang](https://github.com/lewang)
contributed consistent handling of the mark ring, expanding into pairs/quotes
just left of the cursor, and general code clean-up. * [Matt
Briggs](https://github.com/mbriggs) contributed expansions for ruby-mode. *
[Ivan Andrus](https://github.com/gvol) contributed expansions for python-mode,
text-mode, @code{LaTeX-mode} and nxml-mode. * [Raimon
Grau](https://github.com/kidd) added support for when transient-mark-mode is
off. * [Gleb Peregud](https://github.com/gleber) contributed expansions for
erlang-mode. * [fgeller](https://github.com/fgeller) and
[edmccard](https://github.com/edmccard) contributed better support for python
and its multiple modes. * [François Févotte](https://github.com/ffevotte)
contributed expansions for C and C++. * [Roland
Walker](https://github.com/rolandwalker) added option to copy the contents of
the most recent action to a register, and some fixes. * [Damien
Cassou](https://github.com/@code{DamienCassou}) added option to continue
expanding/contracting with fast keys after initial expand.  Thanks!")
    (license #f)))

(define-public emacs-epl
  (package
    (name "emacs-epl")
    (version "20180205.2049")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cask/epl")
             (commit "78ab7a85c08222cd15582a298a364774e3282ce6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ksilx9gzdazngxfni5i632jpb1nprcxplsbhgqirs2xdl53q8v8"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "http://github.com/cask/epl")
    (synopsis "Emacs Package Library")
    (description
     "This package provides a package management library for Emacs, based on
package.el.  The purpose of this library is to wrap all the quirks and hassle of
package.el into a sane API. The following functions comprise the public
interface of this library: ; Package directory selection `epl-package-dir gets
the directory of packages. `epl-default-package-dir gets the default package
directory. `epl-change-package-dir changes the directory of packages. ; Package
system management `epl-initialize initializes the package system and activates
all packages. `epl-reset resets the package system. `epl-refresh refreshes all
package archives. `epl-add-archive adds a new package archive. ; Package objects
Struct `epl-requirement describes a requirement of a package with `name and
`version slots. `epl-requirement-version-string gets a requirement version as
string.  Struct `epl-package describes an installed or installable package with
a `name and some internal `description'. `epl-package-version gets the version
of a package. `epl-package-version-string gets the version of a package as
string. `epl-package-summary gets the summary of a package.
`epl-package-requirements gets the requirements of a package.
`epl-package-directory gets the installation directory of a package.
`epl-package-from-buffer creates a package object for the package contained in
the current buffer. `epl-package-from-file creates a package object for a
package file, either plain lisp or tarball. `epl-package-from-descriptor-file
creates a package object for a package description (i.e. *-pkg.el) file. ;
Package database access `epl-package-installed-p determines whether a package is
installed, either built-in or explicitly installed. `epl-package-outdated-p
determines whether a package is outdated, that is, whether a package with a
higher version number is available. `epl-built-in-packages',
`epl-installed-packages', `epl-outdated-packages and `epl-available-packages get
all packages built-in, installed, outdated, or available for installation
respectively. `epl-find-built-in-package', `epl-find-installed-packages and
`epl-find-available-packages find built-in, installed and available packages by
name. `epl-find-upgrades finds all upgradable packages. `epl-built-in-p return
true if package is built-in to Emacs. ; Package operations `epl-install-file
installs a package file. `epl-package-install installs a package.
`epl-package-delete deletes a package. `epl-upgrade upgrades packages.")
    (license #f)))

(define-public emacs-load-env-vars
  (package
    (name "emacs-load-env-vars")
    (version "20180511.2210")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diasjorge/emacs-load-env-vars")
             (commit "3808520efaf9492033f6e11a9bffd68eabf02a0f")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yw1ihns914k8va5mhphch1zix09x22cpgrbw968mh8hviknyvzr"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/diasjorge/emacs-load-env-vars")
    (synopsis "Load environment variables from files")
    (description
     "This package allows you set environment variables loaded from a file with bash
style variable declarations.  Supported syntax: export KEY=VALUE KEY=VALUE
KEY='VALUE KEY=\"VALUE\" # Comment lines are ignored KEY=VALUE # Inline comments
are ignored KEY: VALUE.")
    (license #f)))

(define-public emacs-gntp
  (package
    (name "emacs-gntp")
    (version "20141025.250")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tekai/gntp.el")
             (commit "767571135e2c0985944017dc59b0be79af222ef5")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nvyjjjydrimpxy4cpg90si7sr8lmldbhlcm2mx8npklp9pn5y3a"))))
    (build-system emacs-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/tekai/gntp.el")
    (synopsis "Growl Notification Protocol for Emacs")
    (description
     "This package implements the Growl Notification Protocol GNTP described at
http://www.growlforwindows.com/gfw/help/gntp.aspx It is incomplete as it only
lets you send but not receive notifications.")
    (license #f)))

(define-public emacs-request-deferred
  (package
    (name "emacs-request-deferred")
    (version "20220614.1604")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tkf/emacs-request")
             (commit "c22e3c23a6dd90f64be536e176ea0ed6113a5ba6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15affk5cp6va3d8wf8567l45nri4ayiwk52p7i40h7nafjq4wp04"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-deferred emacs-request))
    (arguments
     '(#:tests? #f #:include '("^request-deferred.el$")
       #:exclude '()))
    (home-page "https://github.com/tkf/emacs-request")
    (synopsis "Wrap request.el by deferred")
    (description
     "Trivial wrapper to request library returing kiwanami deferred object.")
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

(define-public emacs-burly
  (package
    (name "emacs-burly")
    (version "20240727.545")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alphapapa/burly.el")
             (commit "d5b7133b5b629dd6bca29bb16660a9e472e82e25")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18qq5zsmb9gg701158dwx7qkh2l5m4m029lmgmydchi9xb2g9as9"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-map))
    (arguments '(#:tests? #f))
    (home-page "https://github.com/alphapapa/burly.el")
    (synopsis "Save and restore frame/window configurations with buffers")
    (description
     "This package provides tools to save and restore frame and window configurations
in Emacs, including buffers that may not be live anymore.  In this way, it's
like a lightweight \"workspace\" manager, allowing you to easily restore one or
more frames, including their windows, the windows layout, and their buffers.
Internally it uses Emacs's bookmarks system to restore buffers to their previous
contents and location.  This provides power and extensibility, since many major
modes already integrate with Emacs's bookmarks system.  However, in case a
mode's bookmarking function isn't satisfactory, Burly allows the user to
customize buffer-restoring functions for specific modes.  For Org mode, Burly
provides such custom functions so that narrowed and indirect Org buffers are
properly restored, and headings are located by outline path in case they've
moved since a bookmark was made (the org-bookmark-heading package also provides
this through the Emacs bookmark system, but users may not have it installed, and
the functionality is too useful to not include here by default).  Internally,
buffers and window configurations are also encoded as URLs, and users may also
save and open those URLs instead of using Emacs bookmarks. (The name \"Burly\"
comes from \"buffer URL.\").")
    (license #f)))

(define-public emacs-pretty-hydra
  (package
    (name "emacs-pretty-hydra")
    (version "20250310.2303")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jerrypnz/major-mode-hydra.el")
             (commit "2494d71e24b61c1f5ef2dc17885e2f65bf98b3b2")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqjjfl2x77w1jw9i75w0ghax050df0acsmscxy0rsz6fa1x90az"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-hydra emacs-s emacs-dash emacs-compat))
    (arguments
     '(#:tests? #f #:include '("^pretty-hydra.el$")
       #:exclude '()))
    (home-page "https://github.com/jerrypnz/major-mode-hydra.el")
    (synopsis "A macro for creating nice-looking hydras")
    (description
     "This package provides a macro, `pretty-hydra-define', which defines a hydra with
column for each group of heads.")
    (license #f)))

(define-public emacs-llama
  (package
    (name "emacs-llama")
    (version "20260101.1830")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tarsius/llama")
             (commit "2a89ba755b0459914a44b1ffa793e57f759a5b85")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fcribk74shqz757b8i4cybpia7j3x886lxfa5vlzxc3wwlf3x37"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat))
    (arguments
     '(#:tests? #f #:include '("^llama.el$" "^.dir-locals.el$")
       #:exclude '()))
    (home-page "https://github.com/tarsius/llama")
    (synopsis "Compact syntax for short lambda")
    (description
     "This package implements a macro named `##', which provides a compact way to
write short `lambda expressions.  The signature of the macro is (## FN &rest
BODY) and it expands to a `lambda expression, which calls the function FN with
the arguments BODY and returns the value of that.  The arguments of the `lambda
expression are derived from symbols found in BODY. Each symbol from `%1 through
`%9', which appears in an unquoted part of BODY, specifies a mandatory argument.
 Each symbol from `&1 through `&9', which appears in an unquoted part of BODY,
specifies an optional argument.  The symbol `&* specifies extra (`&rest')
arguments.  The shorter symbol `% can be used instead of `%1', but using both in
the same expression is not allowed.  Likewise `& can be used instead of `&1'.
These shorthands are not recognized in function position.  To support binding
forms that use a vector as VARLIST (such as `-let from the `dash package),
argument symbols are also detected inside of vectors.  The space between `## and
FN can be omitted because `## is read-syntax for the symbol whose name is the
empty string.  If you prefer you can place a space there anyway, and if you
prefer to not use this somewhat magical symbol at all, you can instead use the
alternative name `llama'.  Instead of: (lambda (a &optional _ c &rest d) (foo a
(bar c) d)) you can use this macro and write: (##foo %1 (bar &3) &*) which
expands to: (lambda (%1 &optional _&2 &3 &rest &*) (foo %1 (bar &3) &*)) Unused
trailing arguments and mandatory unused arguments at the border between
mandatory and optional arguments are also supported: (##list %1 _%3 &5 _&6)
becomes: (lambda (%1 _%2 _%3 &optional _&4 &5 _&6) (list %1 &5)) Note how `_%3
and `_&6 are removed from the body, because their names begin with an
underscore.  Also note that `_&4 is optional, unlike the explicitly specified
`_%3'.  Consider enabling `llama-fontify-mode to highlight `## and its special
arguments.")
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

(define-public emacs-xdg-launcher
  (package
    (name "emacs-xdg-launcher")
    (version "20251129.2038")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-exwm/xdg-launcher")
             (commit "1592f8ee3dc26180112411e7f983cafba88024fa")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mpw9cc01hb75yl8gzn8kbx5wbaz1l0gh75cpjv42i2d021bc59z"))))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/emacs-exwm/xdg-launcher")
    (synopsis "XDG application launcher for emacs")
    (description "XDG launcher implements a dmenu-style XDG application launcher in Emacs using
standard Emacs minibuffer completion. It works best with a vertical completion
framework like ~icomplete-vertical-mode~, ~fido-vertical-mode~, ~vertico~, etc. ")
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

(define-public emacs-treesit-fold
  (package
    (name "emacs-treesit-fold")
    (properties '((commit . "ec7e5b02e5d0f0902e19a42e5af4801817598fa3")))
    (version (git-version "0.2.1" "1" (assoc-ref properties 'commit)))
    (home-page "https://github.com/emacs-tree-sitter/treesit-fold")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url home-page)
              (commit (assoc-ref properties 'commit))))
       (sha256
        (base32 "15vybnsw4as1sk04czxz7dq1w3q5bc7837l9gfb3hzgp4ksgg8dd"))))
    (arguments (list #:tests? #f))
    (build-system emacs-build-system)
    (synopsis "Code folding using treesit")
    (description "fold using treesit")
    (license license:gpl3+)))

(define-public emacs-bind-key
  (package
    (name "emacs-bind-key")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/bind-key-" version
                           ".tar"))
       (sha256
        (base32 "0jrbm2l6h4r7qjcdcsfczbijmbf3njzzzrymv08zanchmy7lvsv2"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/jwiegley/use-package")
    (synopsis "A simple way to manage personal keybindings")
    (description synopsis)
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

(define-public emacs-evil-textobj-anyblock
  (package
    (name "emacs-evil-textobj-anyblock")
    (version "20170905.1907")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/willghatch/evil-textobj-anyblock")
             (commit "29280cd71a05429364cdceef2ff595ae8afade4d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1009nmwkdk97hl6pnhlay234gx3krpgapqxj9nqfr7cwl3z89pc0"))))
    (arguments (list #:tests? #f))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-evil))
    (home-page "https://github.com/noctuid/evil-textobj-anyblock")
    (synopsis "Textobject for the closest user-defined blocks")
    (description
     "This package is a port of vim-textobj-anyblock.  It gives text objects for the
closest block of those defined in the evil-anyblock-blocks alist.  By default it
includes (), {}, [], <>, , \"\", ``, and “”.  This is convenient for operating on
the closest block without having to choose between typing something like i{ or
i<.  This package allows for the list of blocks to be changed.  They can be more
complicated regexps.  A simple expand-region like functionality is also provided
when in visual mode, though this is not a primary focus of the plugin and does
not exist in vim-textobj-anyblock.  Also, in the case that the point is not
inside of a block, anyblock will seek forward to the next block.  The required
version of evil is based on the last change I could find to evil-select-paren,
but the newest version of evil is probably preferable.  For more information see
the README in the github repo.")
    (license #f)))

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
