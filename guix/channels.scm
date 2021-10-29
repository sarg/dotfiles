(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))

       (channel
        (name 'personal)
        (url "file:///home/sarg/.config/guix/personal"))

       ;; (channel
       ;;  (name 'pkill9-free)
       ;;  (url "https://gitlab.com/pkill-9/guix-packages-free"))

       ;; (channel
       ;;  (name 'guix-gaming-games)
       ;;  (url "https://gitlab.com/guix-gaming-channels/games.git")
       ;;  ;; Enable signature verification:
       ;;  (introduction
       ;;   (make-channel-introduction
       ;;    "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
       ;;    (openpgp-fingerprint
       ;;     "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))

       (channel
        (name 'guix)
        (branch "master")
        (url "https://git.savannah.gnu.org/git/guix.git"))

       '())
