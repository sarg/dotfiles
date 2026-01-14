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
