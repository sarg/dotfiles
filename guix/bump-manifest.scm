(use-modules (guix packages)
             (gnu)
             (personal packages doomemacs)
             (personal packages quake3e)
             (personal packages scrcpy)
             (personal packages haskell)
             (personal packages wm)
             (personal packages xlibre)
             (personal packages binary))

(packages->manifest
 (list scrcpy scrcpy-server
       xlibre-server
       atuin
       oama
       python-ty
       tinymediamanager
       noctalia-shell
       noctalia-qs
       github-cli
       google-gemini-cli))
