;;; init.el -*- lexical-binding: t; -*-

(defvar org-directory (expand-file-name "~/Sync/org/"))
(doom!
 :completion  company (ivy +fuzzy)
 :ui          nav-flash modeline ophints (popup +all +defaults) pretty-code unicode vc-gutter window-select treemacs
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors
 :emacs       dired electric vc
 :term        eshell shell term vterm
 :tools       editorconfig eval flycheck (lookup +docsets) magit pdf (pass +auth) docker
 :lang        clojure data hy emacs-lisp (org +dragndrop +ipython +pandoc +present) rest (sh +fish) plantuml
 :app         calendar (telega +ivy) (rss +org) sauron
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
