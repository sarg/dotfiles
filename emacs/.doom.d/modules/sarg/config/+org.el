(defun sarg/init-org-protocol ()
  ;; initialize org-protocol for capturing from browser

  (require 'org-protocol)

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


(defun sarg/org-init ()
  (sarg/init-org-protocol)

  (require 'org-contacts)
  (setq-default org-contacts-files (list (expand-file-name "contacts.org" org-directory)))

  (setq
   org-src-window-setup 'current-window
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
                        ((expand-file-name "notes.org" org-directory) :maxlevel . 9)
                        ((expand-file-name "someday.org" org-directory) :maxlevel . 1))

   org-outline-path-complete-in-steps nil ; Refile in a single go
   org-refile-use-outline-path t


   ;; syntax highlight in code blocks
   org-src-fontify-natively t

   ;; org-mode capture templates
   org-capture-templates
   `(("t" "TODO" entry (file ,(expand-file-name "inbox.org" org-directory))

      "* TODO %?\n %i\n %a")
     ;; ("j" "Journal" entry (file+datetree "~/Sync/org/dated.org")
     ;;  "* %?\n%U\n")

     ("w" "work entry" entry (file ,(expand-file-name "work.org" org-directory))
      "* TODO %?\n %i\n %a")

     ;; ("p" "process-soon" entry (file+headline "~/Sync/org/notes.org" "Inbox")
     ;;  "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
     ;; ;; "* TODO %?\n %i\n %a")
     )


   ;; Prettier bullets
   org-bullets-bullet-list '("■" "◆" "▲" "▶")

   ;; agenda
   org-agenda-files
   (-map (lambda (el) (expand-file-name el org-directory))
         '("projects.org"
           "tickler.org"))

   org-catch-invisible-edits 'show-and-error

   linkmarks-file (expand-file-name "links.org" org-directory)))

(use-package! es-mode
  :commands (org-babel-execute:es)

  :config
  (appendq!
   +org-babel-mode-alist '((es . elasticsearch))))

(after! org-download
  (setq org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))

(use-package! linkmarks)

(add-hook! 'after-init-hook #'sarg/org-init)
