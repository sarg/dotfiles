(define-module (personal packages vvvvvv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (guix git-download))

(define-public vvvvvv
  (package
    (name "vvvvvv")
    (version "2021-01-16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/TerryCavanagh/VVVVVV.git")
                    (commit "74740c5a21c05116b92deba8e462f815232fd3bc")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04xdcq0mmdr9zmmbl2s3viysnkj720aw2n6sma1683gajvc00ili"))))
    
    (build-system cmake-build-system)
    (inputs `(("sdl" ,(sdl-union (list sdl2 sdl2-mixer)))))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list
        "-DSDL2_LIBRARIES=SDL2;SDL2_mixer"
        (string-append "-DSDL2_INCLUDE_DIRS=" (assoc-ref %build-inputs "sdl") "/include/SDL2"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _ (chdir "desktop_version") #t))

         (replace 'install
           ;; Upstream provides no install phase.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "VVVVVV" bin))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Governikus/AusweisApp2")
    (synopsis "")
    (description "")
    (license #f)))

vvvvvv
