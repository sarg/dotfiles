(after! ivy
  (ivy-add-actions
   '+pass/qute
   '(("o" +pass/copy-secret "copy password")
     ("e" +pass/edit-entry "edit entry")
     ("u" +pass/copy-user "copy username")
     ("b" +pass/copy-url "open url in browser")
     ("t" password-store-otp-token-copy "copy otp token")
     ("f" +pass/copy-field "get field"))))

(setq password-cache-expiry (* 60 15))

;; for magithub auth to work create pass entry user^magithub@api.github.com
(after! magit
    (setq magit-process-find-password-functions '(magit-process-password-auth-source)))
