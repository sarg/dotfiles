#!/bin/sh

# setxkbmap -option ""
# setxkbmap us,ru -option grp:ctrl_shift_toggle -option caps:shift -option ctrl:swap_lalt_lctl

# hwdb
# /etc/udev/hwdb.d/63-remap.hwdb 
# evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvr*
# KEYBOARD_KEY_1d=leftalt
# KEYBOARD_KEY_38=leftctrl

# https://www.emacswiki.org/emacs/StickyModifiers
cat <<EOF | xkbcomp -w 0 - $DISPLAY
xkb_keymap {
	xkb_keycodes  { include "evdev+aliases(qwerty)"	};
	xkb_types     { include "complete"	};
	xkb_compat    { include "complete"	};
	xkb_symbols   {
    include "pc+us+ru:2+inet(evdev)+group(ctrl_shift_toggle)"
    replace key <CAPS> { [ Shift_L ] };
    replace key <PRSC> { [ Menu ] };
  };
	xkb_geometry  { include "pc(pc105)"	};
};
EOF

xkbset -bell -feedback sticky -twokey latchlock
xkbset exp 64 '=sticky' '=twokey' '=latchlock'
