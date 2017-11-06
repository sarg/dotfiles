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

  ;; org-contacts
  (require 'org-contacts)
  (setq-default org-contacts-files '("~/Sync/org/contacts.org"))

  (setq-default
   ;; don't ask to evaluate code block
   org-confirm-babel-evaluate nil

   setq org-startup-indented t

   ;; experimental
   org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
   org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))

   ;; Include current clocking task in clock reports
   org-clock-report-include-clocking-task t

   org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

   ;; custom agenda
   ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
   org-agenda-custom-commands
   '(("d" "Daily agenda and all TODOs"
      ((tags "PRIORITY=\"A\""
             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
              (org-agenda-overriding-header "High-priority unfinished tasks:")))
       (agenda "" ((org-agenda-span 'day)))
       (alltodo ""
                ((org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-overriding-header "ALL normal priority tasks:"))))
      ((org-agenda-compact-blocks t))))

   org-plantuml-jar-path "~/.local/share/plantuml/plantuml.jar"

   org-refile-use-outline-path 'file
   org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)
                        ("~/Sync/org/notes.org" :maxlevel . 9)
                        ("~/Sync/org/someday.org" :maxlevel . 1)
                        )
   org-outline-path-complete-in-steps nil         ; Refile in a single go
   org-refile-use-outline-path t


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
