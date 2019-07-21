(after! password-store
  (advice-add #'password-store-copy :override #'+pass/copy-secret))

(defun +pass/qute (url)
    (auth-source-pass--read-entry
     (ivy-read "Pass: "
               (password-store-list)
               :initial-input url
               :caller '+pass/qute)))

(after! ivy
  (ivy-add-actions
   '+pass/qute
   '(("o" +pass/copy-secret "copy password")
     ("e" +pass/edit-entry "edit entry")
     ("u" +pass/copy-user "copy username")
     ("b" +pass/copy-url "open url in browser")
     ("t" password-store-otp-token-copy "copy otp token")
     ("f" +pass/copy-field "get field"))))

(defalias '+pass/read-entry #'auth-source-pass--read-entry)

(setq password-cache-expiry (* 60 15))

;; for magithub auth to work create pass entry user^magithub@api.github.com
(after! magit
    (setq magit-process-find-password-functions '(magit-process-password-auth-source)))