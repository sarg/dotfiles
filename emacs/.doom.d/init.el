;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion  company (ivy +fuzzy +prescient +childframe)
 :ui          nav-flash modeline ophints (popup +all +defaults) pretty-code unicode vc-gutter window-select treemacs
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors
 :emacs       dired electric vc ibuffer
 :term        eshell shell term vterm
 :tools       editorconfig (eval +overlay) (lookup +docsets) magit pdf (pass +auth) docker lsp debugger
 :checkers    syntax
 :lang        (cc +lsp) clojure data hy emacs-lisp (org +dragndrop +ipython +pandoc +present) rest (sh +fish) plantuml (python +pyenv +lsp) scheme
 :app         calendar (telega +ivy) (rss +org) sauron (emms +volume +spotify)
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
