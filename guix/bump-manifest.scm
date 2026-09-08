(use-modules (guix packages)
             (gnu)
             (personal packages quake3e)
             (personal packages scrcpy)
             (personal packages ghostty)
             (personal packages haskell)
             (personal packages xlibre)
             (personal packages binary))

(packages->manifest
 (list scrcpy scrcpy-server
       emacs-ghostel
       oama
       python-ty
       ;; tinymediamanager
       ;; opencode
       ))
