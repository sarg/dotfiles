(define-module (personal packages xlibre)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix modules)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml))

(define-public xlibre-server
  (package
    (name "xlibre-server")
    (version "25.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/X11Libre/xserver")
             (commit (string-append "xlibre-xserver-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0is0akylbq3iam3a0fxwa7xxm44h5bygy1crrnk54z9k7vam5njk"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; tests don't run in chroot because of Popen call (os/utils.c) which drops privileges
      ;; after dropping privs xkbcomp binary is not visible to the forked process
      ;; substituting Popen/Pclose with popen/pclose fixes the issue but is not great from security standpoint
      #:tests? #f
      #:configure-flags #~(list "-Dxcsecurity=true"
                                "-Dxkb_output_dir=/tmp"
                                "-Dxephyr=true")))
    (propagated-inputs
     ;; The following libraries are required by xorg-server.pc.
     (list libpciaccess libxcvt mesa libxfont2 pixman xorgproto))
    (inputs (list eudev
                  dbus
                  libdmx
                  libepoxy
                  libgcrypt
                  libseat
                  libxau
                  libxaw
                  libxdmcp
                  libxfixes
                  libxfont2
                  libxkbfile
                  libxrender
                  libxres
                  libxshmfence
                  libxt
                  libxv
                  xkbcomp
                  xkeyboard-config
                  xtrans
                  zlib
                  ;; Inputs for Xephyr
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm))
    (native-inputs (list pkg-config python-wrapper
                         ;; for tests
                         xrandr xvinfo xdpyinfo))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description
     "This package provides the Xorg X server itself.
The X server accepts requests from client programs to create windows, which
are (normally rectangular) 'virtual screens' that the client program can
draw into.

Windows are then composed on the actual screen by the X server (or by a
separate composite manager) as directed by the window manager, which usually
communicates with the user via graphical controls such as buttons and
draggable titlebars and borders.")
    (license license:x11)))

(define-public xlibre-xf86-input-libinput
  (package
    (name "xlibre-xf86-input-libinput")
    (version "25.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/X11Libre/xf86-input-libinput")
              (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11dm4igrbyy15az929m0sdzgx29pnbvnsc0vhyjv6cllfhf5x29w"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "-Dsdkdir=" %output "/include/xorg")
             (string-append "-Dxorg-conf-dir=" %output "/share/X11/xorg.conf.d")
             (string-append "-Dxorg-module-dir=" %output "/lib/xorg/modules/input"))))
    (native-inputs (list pkg-config))
    (inputs (list libinput-minimal xlibre-server))
    (home-page "https://github.com/X11Libre/xf86-input-libinput")
    (synopsis "Input driver for X server based on libinput")
    (description
     "xf86-input-libinput is an input driver for the Xorg X server based
on libinput.  It is a thin wrapper around libinput, so while it does
provide all features that libinput supports it does little beyond.")
    (license license:x11)))
