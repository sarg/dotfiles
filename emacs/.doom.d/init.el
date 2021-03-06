;;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion  company (ivy +fuzzy +prescient +childframe)
 :ui          nav-flash (modeline +light) ophints (popup +all +defaults) pretty-code hydra
 vc-gutter window-select treemacs workspaces zen
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors
 :emacs       dired electric vc ibuffer undo
 :term        eshell shell term vterm
 :tools       editorconfig (eval +overlay) (lookup +docsets) (magit +forge) pdf (pass +auth) docker lsp debugger direnv make
 :checkers    syntax
 :lang        (cc +lsp) clojure data hy emacs-lisp (go +lsp)
 (org +dragndrop +ipython +pandoc +present +hugo +roam)
 rest sh plantuml (python +pyenv +lsp) scheme (web +html) yaml json
 :app         calendar (telega +ivy) (rss +org) sauron (emms +volume +spotify)
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
