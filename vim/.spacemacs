;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   '(nginx
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     javascript
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
     java
     lua
     puppet
     erc
     python
     pass
     emacs-lisp
     themes-megapack
     ranger
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
   dotspacemacs-additional-packages '(exwm
                                      webpaste
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(mu4e-maildirs-extension)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
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
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(soft-morning
                         material-light
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code 12"
                               :spacing 0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
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
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
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
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
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
   dotspacemacs-fullscreen-at-startup nil
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
   dotspacemacs-line-numbers nil
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
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; separate custom settings
  (setq custom-file "~/.emacs.d/private/custom.el")
  (load custom-file 'noerror)
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
   mu4e-update-interval (* 60 15)

   ;; notification settings
   mu4e-enable-notifications t
   mu4e-enable-mode-line t
   mu4e-alert-interesting-mail-query (concat "flag:unread "
                                             "AND NOT flag:trashed "
                                             "AND (maildir:/srg/Inbox OR maildir:/gmail/Inbox) "
                                             "AND NOT from:srgn "
                                             "AND NOT from:bitb "
                                             "AND NOT from:jira "
                                             )
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
               (mu4e-sent-folder . "/srg/sent")
               (mu4e-drafts-folder . "/srg/drafts")
               (mu4e-trash-folder . "/srg/trash")
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
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (require 'exwm)
  (require 'exwm-config)

  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))
  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)

     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))
  ;; Configure Ido
  (exwm-config-ido)
  ;; Other configurations
  (exwm-config-misc)

  (setq exwm-layout-show-all-buffers t
        exwm-workspace-show-all-buffers t
        )

  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))

  ;; C-h deletes character backwards
  (define-key key-translation-map [?\C-h] [?\C-?])

  ;; company mode everywhere
  (global-company-mode)

  ;; pcre regexes
  (pcre-mode)

  ;; enable evil-snipe
  (setq evil-snipe-enable-alternate-f-and-t-behaviors t)

  ;; use ranger instead of dired-jump
  (ranger-override-dired-mode t)
  (setq ranger-show-hidden nil)

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
    (require 'org-protocol)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sql . t)
       (emacs-lisp . t)
       (plantuml . t)
       (maxima . t)
       (octave . t)
       (http . t)
       (shell . t)
       (dot . t)
       (org . t)
       (python . t)))

    (setq 
     ;; don't ask to evaluate code block
     org-confirm-babel-evaluate nil)

    (setq org-startup-indented t)

    ;; experimental
    (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00"))))

    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

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


    ;; org-contacts
    (require 'org-contacts)
    (setq-default org-contacts-files '("~/Sync/org/contacts.org"))



    (setq-default
     org-plantuml-jar-path "~/.local/share/plantuml/plantuml.jar"

     org-refile-use-outline-path 'file
     org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9)
                          ("~/Sync/org/notes.org" :maxlevel . 9)
                          ("~/Sync/org/someday.org" :level . 1)
                          )
     org-outline-path-complete-in-steps nil         ; Refile in a single go
     org-refile-use-outline-path t

     ;; set browser
     browse-url-browser-function 'browse-url-generic
     browse-url-generic-program "qutebrowser"

     ;; syntax highlight in code blocks
     org-src-fontify-natively t

     ;; org-mode capture templates
     org-capture-templates
     '(("t" "TODO" entry (file "~/Sync/org/inbox.org")

        "* TODO %?\n %i\n %a")
       ("j" "Journal" entry (file+datetree "~/Sync/org/dated.org")
        "* %?\n%U\n")

       ("w" "work entry" entry (file "~/Sync/org/work.org")
        "* TODO %?\n %i\n %a")

       ;; ("p" "process-soon" entry (file+headline "~/Sync/org/notes.org" "Inbox")
       ;;  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
       ;; ;; "* TODO %?\n %i\n %a")
       )

     ;; don't split heading on M-RET in the middle of line
     org-M-RET-may-split-line nil

     ;; Prettier bullets
     org-bullets-bullet-list '("■" "◆" "▲" "▶")

     ;; agenda
     org-agenda-files '("~/Sync/org/work.org"
                        "~/Sync/org/mirea.org"
                        "~/Sync/org/projects.org"
                        "~/Sync/org/teztour.org"
                        "~/Sync/org/tickler.org"
                        )

     org-catch-invisible-edits 'show-and-error

     )

    ;; enable auto-fill for org-mode
    (add-hook 'org-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

    ;; start capturing in insert state
    (add-hook 'org-capture-mode-hook 'evil-insert-state)
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


  (setq frame-title-format "%b - emacs")


  ;; use zathura when ! on file in dired
  (setq dired-guess-shell-alist-user
          '(("\\.pdf" "zathura")))

  (setq delete-by-moving-to-trash nil)


  (setq-default
   ;; russian layout on C-\
   default-input-method "russian-computer"

   ;; fixes epa-file--find-file-not-found-function: Opening input file: Decryption failed, 
   ;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
   epa-pinentry-mode 'loopback

   ;; escape to normal with jk
   evil-escape-key-sequence "jk"

   ;; follow symbolic links under version control with a warning
   vc-follow-symlinks nil

   ;; disable increased heading size in spacemacs-light theme
   spacemacs-theme-org-height nil

   hybrid-mode-enable-hjkl-bindings t)


  (spacemacs/set-leader-keys "a m" 'mu4e)
  (with-eval-after-load 'mu4e (mu4e-context-setup))

  )
