;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     yaml
     html
     sql
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ivy
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     better-defaults
     restclient
     lua
     puppet
     erc
     python
     emacs-lisp
     git
     markdown
     mu4e
     (org :variables
          org-enable-org-journal-support t
          org-journal-dir "~/Sync/org/journal/"
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format "%R"

          org-directory "~/Sync/org")
     (shell :variables
            shell-default-height 30
            shell-default-shell 'eshell
            shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     version-control
     vinegar
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(mu4e-maildirs-extension)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code 12"
                               :spacing 0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'nil
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun mu4e-context-setup ()
  (defun mu4e-msg-to-me (msg)
    "Is message sent to me?"
    (when msg
      (or (mu4e-message-contact-field-matches-me msg :to)
          (mu4e-message-contact-field-matches-me msg :bcc)
          (mu4e-message-contact-field-matches-me msg :cc)
          )
      ))

  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; if rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  (fset 'mu4e-move-to-trash "mt")

  (define-key mu4e-main-mode-map (kbd "u") 'mu4e-update-mail-and-index)
  (mu4e-alert-set-default-style 'notifications)

  (setq
   mu4e-maildir "~/.mail"

   mu4e-get-mail-command "fetchnewmail"
   mu4e-update-interval 300

   ;; notification settings
   mu4e-enable-notifications t
   mu4e-enable-mode-line t
   mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND (maildir:/srg/Inbox OR maildir:/gmail/Inbox)"

   ;; mu4e-html2text-command "html2text -utf8 -nobs -width 72"
   ;; mu4e-html2text-command "w3m -T text/html"

   ;; display images
   mu4e-view-show-images t

   ;; iso date format
   mu4e-headers-date-format "%F"

   ;; column list for Headers view
   mu4e-headers-fields '(
                         (:human-date . 12)
                         (:flags . 6)
                         (:from . 22)
                         (:subject)
                         )

   ;; close sent message buffers
   message-kill-buffer-on-exit t

   ;; pick first context automatically on launch
   mu4e-context-policy               'pick-first
   ;; use current context for new mail
   mu4e-compose-context-policy       'ask-if-none
   mu4e-confirm-quit                 nil

   ;; mbsync goes crazy without this setting
   mu4e-change-filenames-when-moving t

   ;; bookmarks
   mu4e-bookmarks '(("flag:unread AND NOT flag:trashed AND (maildir:/srg/Inbox OR maildir:/gmail/Inbox)" "Unread messages" 117)
                    ("date:today..now" "Today's messages" 116))


   ;; Configure sending mail.
   message-send-mail-function 'message-send-mail-with-sendmail
   sendmail-program "/usr/bin/msmtp"
   user-full-name "Sergey Trofimov"

   ;; Use the correct account context when sending mail based on the from header.
   message-sendmail-envelope-from 'header

   ;; send with msmtp
   send-mail-function 'sendmail-send-it

   mu4e-contexts
   `(,(make-mu4e-context
       :name "gmail"
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-maildir-matches msg "^/gmail")))
       :enter-func (lambda () (define-key mu4e-headers-mode-map (kbd "d") 'mu4e-move-to-trash))
       :leave-func (lambda () (define-key mu4e-headers-mode-map (kbd "d") 'mu4e-headers-mark-for-trash))
       :vars '(
               ;; local directories, relative to mail root
               (mu4e-sent-folder . "/gmail/sent")
               (mu4e-drafts-folder . "/gmail/drafts")
               (mu4e-trash-folder . "/gmail/trash")
               (mu4e-refile-folder . "/gmail/all")
               ;; account details
               (user-mail-address . "sarg@sarg.org.ru")
               (user-full-name . "Sergey Trofimov")
               (mu4e-user-mail-address-list . ( "sarg@sarg.org.ru" ))
               ;; gmail saves every outgoing message automatically
               (mu4e-sent-messages-behavior . delete)
               (mu4e-maildir-shortcuts . (("/gmail/Inbox" . ?j)
                                          ("/gmail/all" . ?a)
                                          ("/gmail/trash" . ?t)))
               (mu4e-headers-skip-duplicates . t)
               ))
     ,(make-mu4e-context
       :name "srg"
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-maildir-matches msg "^/srg")))
       :vars '(
               ;; local directories, relative to mail root
               (mu4e-sent-folder . "/srg/Sent")
               (mu4e-drafts-folder . "/srg/Drafts")
               (mu4e-trash-folder . "/srg/Trash")
               (mu4e-refile-folder . "/srg/Inbox")
               ;; account details
               (user-mail-address . "trofimovsi@srgroup.ru")
               (user-full-name . "Sergey Trofimov")
               (mu4e-user-mail-address-list . ( "trofimovsi@srgroup.ru" ))
               (mu4e-sent-messages-behavior . delete)))))

  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)


  ;; Choose account label to feed msmtp -a option based on From header
  ;; in Message buffer; This function must be added to
  ;; message-send-mail-hook for on-the-fly change of From address before
  ;; sending message since message-send-mail-hook is processed right
  ;; before sending message.
  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "sarg@sarg.org.ru" from) "gmail")
                 ((string-match "trofimovsi@srgroup.ru" from) "srg"))))
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (add-hook 'message-send-mail-hook 'choose-msmtp-account)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; separate custom settings
  (setq custom-file "~/.emacs.d/private/custom.el")
  (load custom-file 'noerror)

  ;; C-h deletes character backwards
  (define-key key-translation-map [?\C-h] [?\C-?])

  ;; company mode everywhere
  (global-company-mode)

  ;; pcre regexes
  (pcre-mode)

  ;; fuzzy match for ivy
  ;; http://oremacs.com/2016/01/06/ivy-flx/
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))

        ivy-initial-inputs-alist nil)

  ;; fix c-w in company mode
  ;; https://github.com/syl20bnr/spacemacs/issues/4243#issuecomment-166246613
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    )

  (with-eval-after-load 'org
    (require 'org-protocol))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (emacs-lisp . t)
     (plantuml . t)
     (http . t)
     (shell . t)
     (dot . t)
     (org . t)
     (python . t)))


  ;; use python3
  (setq python-shell-interpreter "python3")

  ;; fix readline bug:
  ;; http://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
  (with-eval-after-load 'python
    (defun python-shell-completion-native-try ()
      "Return non-nil if can trigger native completion."
      (let ((python-shell-completion-native-enable t)
            (python-shell-completion-native-output-timeout
             python-shell-completion-native-try-output-timeout))
        (python-shell-completion-native-get-completions
         (get-buffer-process (current-buffer))
         nil "_"))))

  (setq 
   ;; don't ask to evaluate code block
   org-confirm-babel-evaluate nil)

  (setq frame-title-format "%b - emacs")

  ;; experimental
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))))

  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  ;; custom agenda
  ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 'day)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks t)))))

  ;; use zathura when ! on file in dired
  (setq dired-guess-shell-alist-user
          '(("\\.pdf" "zathura")))

  (setq-default
   org-plantuml-jar-path "~/.local/share/plantuml/plantuml.jar"

   org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9))
   org-outline-path-complete-in-steps nil         ; Refile in a single go
   org-refile-use-outline-path t

   ;; set browser
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "qutebrowser"

   ;; syntax highlight in code blocks
   org-src-fontify-natively t

   ;; org-mode capture templates
   org-capture-templates
   '(("t" "TODO" entry (file "~/Sync/org/refile.org")

      "* TODO %?\n %i\n %a")
     ("j" "Journal" entry (file+datetree "~/Sync/org/dated.org")
      "* %?\n%U\n")

     ("w" "work entry" entry (file "~/Sync/org/work.org")
      "* TODO %?\n %i\n %a")

     ("p" "process-soon" entry (file+headline "~/Sync/org/notes.org" "Inbox")
      "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
     ;; "* TODO %?\n %i\n %a")
     )

   ;; don't split heading on M-RET in the middle of line
   org-M-RET-may-split-line nil

   ;; Prettier bullets
   org-bullets-bullet-list '("■" "◆" "▲" "▶")

   ;; agenda
   org-agenda-files '("~/Sync/org/")

   ;; russian layout on C-\
   default-input-method "russian-computer"

   ;; fixes epa-file--find-file-not-found-function: Opening input file: Decryption failed, 
   ;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
   epa-pinentry-mode 'loopback

   org-catch-invisible-edits 'show-and-error

   ;; escape to normal with jk
   evil-escape-key-sequence "jk"

   ;; follow symbolic links under version control with a warning
   vc-follow-symlinks nil

   ;; disable increased heading size in spacemacs-light theme
   spacemacs-theme-org-height nil

   hybrid-mode-enable-hjkl-bindings t)

  ;; enable auto-fill for org-mode
  (add-hook 'org-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

  ;; start capturing in insert state
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (spacemacs/set-leader-keys "a m" 'mu4e)
  (with-eval-after-load 'mu4e (mu4e-context-setup))

  ;; http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))
  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (string-inflection yasnippet smartparens magit-popup evil with-editor diminish helm helm-core company counsel projectile magit ivy browse-at-remote yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package unfill toc-org tagedit symon sql-indent spaceline smex smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-delimiters pyvenv pytest pyenv-mode py-isort puppet-mode pug-mode popwin pip-requirements persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-journal org-download org-bullets open-junk-file ob-restclient ob-http neotree mwim multi-term mu4e-alert move-text mmm-mode minitest markdown-toc magit-gitflow macrostep lua-mode lorem-ipsum live-py-mode linum-relative link-hint less-css-mode ivy-purpose ivy-hydra info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav dumb-jump diff-hl define-word cython-mode counsel-projectile company-web company-statistics company-restclient company-anaconda column-enforce-mode clean-aindent-mode chruby bundler auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
