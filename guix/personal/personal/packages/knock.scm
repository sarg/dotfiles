(define-module (personal packages knock)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages image)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix download))

(define-public updfparser
  (let ((revision "0")
        (commit "6060d123441a06df699eb275ae5ffdd50409b8f3"))
    (package
      (name "updfparser")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://forge.soutade.fr/soutade/uPDFParser")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "06fxzdg9ijdbpav6w517qazfs024wm7w29plxv9zsyvqcrcgfghw"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'install
              ;; Upstream provides no install phase.
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (include (string-append out "/include"))
                       (lib (string-append out "/lib")))
                  (mkdir-p lib)
                  (install-file "libupdfparser.so" lib)
                  (mkdir-p include)
                  (copy-recursively "include" include)))))))
      (home-page "http://forge.soutade.fr/soutade/uPDFParser")
      (synopsis "A lightweight PDF parser library")
      (description "uPDFParser is a lightweight PDF parser library written in C++.
It aims to provide a simple and efficient way to extract text and metadata from PDF files.")
      (license license:gpl3+))))

(define-public libgourou
  (package
   (name "libgourou")
   (version "0.8.6")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/vsajip/libgourou.git")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0c571aag1qzh6hx3ig684wsnfibgvw1s1ccn863fbcaq39a56ly5"))
            (modules '((guix build utils)))
            (snippet
             ;; Delete the bundled copy of these libraries.
             '(begin
                (substitute* "utils/Makefile"
                  (("^LDFLAGS.+" a)
                   (string-append a "LDFLAGS += -Wl,-rpath=$(out)/lib\n")))
                (substitute* "Makefile"
                  (("PWD") "CURDIR")
                  (("^LDFLAGS.+" a)
                   (string-append (string-trim-right a) " -lupdfparser\n"))
                  ((" \\$\\(UPDFPARSERLIB\\)") "")
                  (("(UPDFPARSERLIB = ).+" _ a)
                   (string-append a "./Makefile"))
                  (("(all: .+ )lib (.+)" _ a b)
                   (string-append a b)))))))
   (build-system gnu-build-system)
   (inputs (list pugixml libzip openssl curl zlib updfparser))
   (arguments
    (list
     #:tests? #f
     #:make-flags
     #~(list (string-append "PREFIX=" #$output))
     #:phases
     #~(modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'install-headers
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include")))
               (mkdir-p include)
               (copy-recursively "include" include)))))))
   (home-page "http://forge.soutade.fr/soutade/uPDFParser")
   (synopsis "A free ADEPT protocol implementation")
   (description "@code{libgourou} is a free implementation of Adobe's ADEPT protocol used to add DRM on
ePub/PDF files.  It overcomes the lack of Adobe support for Linux platforms.")
   (license license:gpl3+)))

(define-public knock
  (let ((revision "0")
        (commit "6dfb0818216bc62723d745a496e3a79bdf5c3501"))
    (package
      (name "knock")
      (version (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/frnsys/knock.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1540ypyd8fwsb2wmhkyn7ih1ibw2r33w7pkn52v474cmpkgh8v63"))
         (modules '((guix build utils)))
         (snippet '(delete-file-recursively "libgourou"))))
      (arguments
       (list
        #:tests? #f
        #:make-flags
        #~(list (string-append "PREFIX=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'install)
            (replace 'build
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (mkdir-p (string-append #$output "/bin"))
                (invoke "g++" "-o" (string-append #$output "/bin/knock")
                        "--std=c++17"
                        (string-append "-DKNOCK_VERSION=\"" #$version "\"")
                        (string-append "-I" (assoc-ref inputs "libgourou-source") "/utils")
                        "src/knock.cpp"
                        (search-input-file inputs "utils/drmprocessorclientimpl.cpp")
                        "-lgourou" "-lcrypto" "-lzip" "-lcurl" "-lz" "-lpugixml"))))))
      (build-system gnu-build-system)
      (inputs `(("libgourou" ,libgourou)
                ("libgourou-source" ,(package-source libgourou))
                ,@(package-inputs libgourou)))
      (home-page "https://github.com/frnsys/knock")
      (synopsis "Convert ACSM files to PDF/EPUBs with one command")
      (description synopsis)
      (license license:gpl3+))))
