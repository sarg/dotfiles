#!/bin/sh

# setxkbmap -option ""
# setxkbmap us,ru -option grp:ctrl_shift_toggle -option caps:shift -option ctrl:swap_lalt_lctl

# hwdb
# /etc/udev/hwdb.d/63-remap.hwdb 
# evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvr*
# KEYBOARD_KEY_1d=leftalt
# KEYBOARD_KEY_38=leftctrl
# KEYBOARD_KEY_58=leftshift

# https://www.emacswiki.org/emacs/StickyModifiers
cat <<EOF | xkbcomp -w 0 - $DISPLAY
xkb_keymap {
  xkb_keycodes  { include "evdev+aliases(qwerty)"   };
  xkb_types     { include "complete"  };
  xkb_compat    { include "complete"  };
  xkb_symbols   {
    include "pc+us+ru:2+inet(evdev)"
    replace key <PRSC> { [ Menu ] };
  };
  xkb_geometry  { include "pc(pc105)"   };
};
EOF

# xkbset -bell -feedback sticky -twokey latchlock
# xkbset exp 64 '=sticky' '=twokey' '=latchlock'
xset r rate 250 50
xset m 15/10 4

export XMODIFIERS=@im=exwm-xim
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export CLUTTER_IM_MODULE=xim
