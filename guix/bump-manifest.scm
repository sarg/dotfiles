(use-modules (guix packages)
             (gnu)
             (personal packages doomemacs)
             (personal packages quake3e)
             (personal packages scrcpy)
             (personal packages haskell)
             (personal packages xlibre)
             (personal packages binary))

(packages->manifest
 (list scrcpy scrcpy-server
       xlibre-server
       atuin
       oama
       python-ty
       tinymediamanager
       github-cli
       google-gemini-cli))
