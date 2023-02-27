;; init.el -*- lexical-binding: t; -*-

(doom!
 :completion  company vertico ;(ivy +fuzzy +prescient +childframe)
 :ui          nav-flash (modeline +light) ophints (popup +all +defaults) ligatures hydra
 vc-gutter window-select treemacs
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors word-wrap
 :emacs       dired electric vc ibuffer undo
 :term        eshell vterm
 :tools       editorconfig (eval +overlay) (lookup +docsets) (magit) pdf (pass +auth) direnv make tree-sitter
 :checkers    syntax
 :lang        clojure data emacs-lisp
 (org +contacts +dragndrop +pandoc +present +hugo +roam2) beancount
 rest sh plantuml (python +pyenv +lsp) (web +html) yaml json
 :app         calendar telega (rss +org)  wpa
 :email       (mu4e +gmail)
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
