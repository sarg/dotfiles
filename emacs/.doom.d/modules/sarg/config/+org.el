(defun sarg/init-org-protocol ()
  ;; initialize org-protocol for capturing from browser

  (require 'org-protocol)

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
        (delete-frame))))


(defun org-contacts-filter-not-ignored (&rest args)
  (-remove (lambda (contact)
             (-contains?
              (org-split-string
               (or (cdr (assoc-string "ALLTAGS" (caddr contact))) "") ":")
              "ignore"))
           (org-contacts-db)))

(defun sarg/export-contacts-to-vcard ()
  (interactive)
  (cl-letf (((symbol-function 'org-contacts-filter) #'org-contacts-filter-not-ignored))
    (org-contacts-export-as-vcard)))

(after! org

  (sarg/init-org-protocol)

  (require 'org-contacts)
  (setq-default org-contacts-files '("~/Sync/org/contacts.org"))

  (setq
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
     ;; ("j" "Journal" entry (file+datetree "~/Sync/org/dated.org")
     ;;  "* %?\n%U\n")

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

   org-catch-invisible-edits 'show-and-error))

(def-package! linkmarks
  :config

  (setq linkmarks-file (concat org-directory "links.org")))
