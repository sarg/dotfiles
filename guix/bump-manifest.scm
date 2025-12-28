(use-modules (guix packages)
             (gnu)
             (personal packages doomemacs)
             (personal packages quake3e)
             (personal packages scrcpy)
             (personal packages xlibre)
             (personal packages binary))

(packages->manifest
 (list doomemacs
       quake3e
       scrcpy scrcpy-server
       xlibre-server
       atuin python-ty tinymediamanager github-cli))
