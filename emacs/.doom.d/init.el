;;; init.el -*- lexical-binding: t; -*-

(defvar org-directory (expand-file-name "~/Sync/org/"))
(doom!
 :completion  company (ivy +fuzzy)
 :ui          nav-flash modeline ophints (popup +all +defaults) pretty-code unicode vc-gutter window-select
 :editor      (evil +everywhere) fold lispy snippets (format +onsave) multiple-cursors
 :emacs       dired electric
 :term        eshell shell term vterm
 :tools       editorconfig eval flycheck (lookup +docsets) magit pdf (pass +auth)
 :lang        clojure emacs-lisp (org +dragndrop +ipython +pandoc +present) rest (sh +fish)
 :app         calendar (telega +ivy) (rss +org)
 :config      (default +bindings +smartparens)
 :sarg        config exwm)
