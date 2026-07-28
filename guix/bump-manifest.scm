(use-modules (guix packages)
             (gnu)
             (personal packages doomemacs)
             (personal packages quake3e)
             (personal packages scrcpy)
             (personal packages ghostty)
             (personal packages haskell)
             (personal packages xlibre)
             (personal packages binary))

(packages->manifest
 (list scrcpy scrcpy-server
       doomemacs doomemacs-modules
       emacs-ghostel
       oama
       python-ty
       ;; tinymediamanager
       ;; opencode
       ))
