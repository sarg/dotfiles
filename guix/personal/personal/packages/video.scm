;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages video)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mpv-uosc
  (package
    (name "mpv-uosc")
    (version "5.12.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             "https://github.com/tomasklaen/uosc/releases/download/"
             version
             "/uosc.zip"))
       (sha256
        (base32 "0rai7001c5v5jnl1s10kz2a8crysdiclz036n5y4ppjj2nyzcp6f"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/mpv/" #:exclude ("scripts/uosc/bin")))))
    (home-page "https://github.com/tomasklaen/uosc")
    (synopsis "Feature-rich minimalist proximity-based UI for MPV player")
    (description "Feature-rich minimalist proximity-based UI for MPV player.")
    (license license:gpl3+)))

(define-public mpv-thumbfast
  (package
    (name "mpv-thumbfast")
    (properties '((commit . "9deb0733c4e36938cf90e42ddfb7a19a8b2f4641")))
    (version (git-version "0.1" "2" (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/po5/thumbfast")
             (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q537fjj9ndq7pzg2rv4h5qas8s3812k21bpw064bcvb204vbwba"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("thumbfast.lua" "share/mpv/scripts/"))))
    (home-page "https://github.com/po5/thumbfast")
    (synopsis "High-performance on-the-fly thumbnailer script for mpv")
    (description "High-performance on-the-fly thumbnailer script for mpv.")
    (license license:mpl2.0)))

(define-public xob
  (package
    (name "xob")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/florentc/xob")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x4aafiyd9k4y8cmvn7rgfif3g5s5hhlbj5nz71qsyqg21nn7hrw"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                       ; no tests
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "prefix=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (native-inputs (list pkg-config))
    (inputs (list libx11 libxrender libconfig))
    (home-page "https://github.com/florentc/xob")
    (synopsis "X11 overlay bar")
    (description "A lightweight configurable overlay volume/backlight/progress/anything bar for
the X Window System (and Wayland compositors with XWayland).  Each time a new
value is read on the standard input, it is displayed as a tv-like bar over
other windows.  It then vanishes after a configurable amount of time.")
    (license license:gpl3)))
