;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
;;; Copyright © 2025 Meredith Oleander <mereditholeander@gmail.com>
;;; Copyright © 2025 Raven Hallsby <karl@hallsby.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages zig-xyz)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages zig-xyz)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages man)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig))

(define-public libxkbcommon-next
  (package
    (inherit libxkbcommon)
    (name "libxkbcommon")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/xkbcommon/libxkbcommon")
                     (commit (string-append "xkbcommon-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1440s6w7m7xpxw44d3x9wyn5wy4wxdx33idvklxkhp9irnq32jy1"))))
    (native-inputs (modify-inputs (package-native-inputs libxkbcommon)
                     (append setxkbmap)))))

(define-public river-next
  (package
    (inherit river)
    (name "river")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/river/river")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dbxhff773pl4603gg1b39mf9lkhpil93mxsw0fdlgamiwjqnr8h"))))
    (inputs
     (list libevdev
           zig-wayland-0.5.0
           zig-wlroots-0.19.4
           zig-xkbcommon-0.4.0))))

(define-public zig-wayland-0.5.0
  (package
    (inherit zig-wayland)
    (name "zig-wayland")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wayland")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16lnhiminyrmpk02scnnr4zd499d3f0fyah5pn40125s5ns8w6ls"))))))

(define-public zig-wlroots-0.19.4
  (package
    (inherit zig-wlroots)
    (name "zig-wlroots")
    (version "0.19.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-wlroots")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ij289nfivxha8va9hgamg4i82hlgaycwfz59d465317qd4cwll3"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs zig-wlroots)
       (replace "zig-xkbcommon" zig-xkbcommon-next)))))

(define-public zig-xkbcommon-next
  (package
    (name "zig-xkbcommon")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/ifreund/zig-xkbcommon")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "117nw4b5q14mb6j5yhvydlwllbd7gyxp176as4gj9qb5zh8wz5kv"))))
    (build-system zig-build-system)
    (arguments (list #:skip-build? #t))
    (propagated-inputs (list libxkbcommon-next))
    (synopsis "Zig bindings for libxkbcommon")
    (description "This package provides Zig bindings for @code{libxkbcommon}.")
    (home-page "https://codeberg.org/ifreund/zig-xkbcommon")
    (license license:expat)))

(define-public zig-xkbcommon-0.4.0
  (package
    (inherit zig-xkbcommon-next)
    (name "zig-xkbcommon")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://codeberg.org/ifreund/zig-xkbcommon")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18cjrv4gzyihs6cvr1djkb74lj76yn84p65s71ihp11fywzjc2fd"))))))
