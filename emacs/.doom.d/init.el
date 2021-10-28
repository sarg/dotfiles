;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion  company (ivy +fuzzy +prescient +childframe)
 :ui          nav-flash (modeline +light) ophints (popup +all +defaults) ligatures hydra
 vc-gutter window-select treemacs
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors
 :emacs       dired electric vc ibuffer undo
 :term        eshell shell term vterm
 :tools       editorconfig (eval +overlay) (lookup +docsets) (magit) pdf (pass +auth) docker lsp debugger direnv make
 :checkers    syntax
 :lang        (cc +lsp) clojure data hy emacs-lisp (go +lsp)
 (org +dragndrop +ipython +pandoc +present +hugo)
 rest sh plantuml (python +pyenv +lsp) (web +html) yaml json
 :app         calendar (telega +ivy) (rss +org) (emms +volume +spotify)
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
