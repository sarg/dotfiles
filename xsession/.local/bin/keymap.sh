#!/bin/sh

# setxkbmap -option ""
# setxkbmap us,ru -option grp:ctrl_shift_toggle -option caps:shift -option ctrl:swap_lalt_lctl

# hwdb
# /etc/udev/hwdb.d/63-remap.hwdb 
# evdev:atkbd:dmi:bvn*:bvr*:bd*:svn*:pn*:pvr*
# KEYBOARD_KEY_1d=leftalt
# KEYBOARD_KEY_38=leftctrl
# KEYBOARD_KEY_58=leftshift

read -d'' -r backslash_as_layout_switch <<EOF
    replace key <BKSL> {
        type= "LAYOUT_SWITCH",
        symbols[Group1]= [ backslash, bar,   ISO_Next_Group ],
        symbols[Group2]= [ backslash, slash, ISO_Next_Group ]
    };
EOF

if [ "$1" == "enable" ]; then
  patch=$backslash_as_layout_switch
else
  patch=""
fi

# https://www.emacswiki.org/emacs/StickyModifiers
xkbcomp -w 0 - $DISPLAY <<EOF
xkb_keymap {
  xkb_keycodes  { include "evdev+aliases(qwerty)"   };
  xkb_types     {
    include "complete"
    type "LAYOUT_SWITCH" {
        modifiers= Shift+Control;
        map[Shift]= Level2;
        map[Control]= Level3;
        level_name[Level1]= "Base";
        level_name[Level2]= "Shift";
        level_name[Level3]= "Control";
    };
  };
  xkb_compat    { include "complete"  };
  xkb_symbols   {
    include "pc+us+ru:2+inet(evdev)"
    $patch
    replace key <LWIN> { [ F13 ] };
    replace key <PRSC> { [ F13 ] };
  };
  xkb_geometry  { include "pc(pc105)"   };
};
EOF

# xkbset -bell -feedback sticky -twokey latchlock
# xkbset exp 64 '=sticky' '=twokey' '=latchlock'
xset -r 133 # disable repeat for F13
xset -r 107 # disable repeat for F13
xset r rate 250 50
xset m 15/10 4

# export XMODIFIERS=@im=exwm-xim
# export GTK_IM_MODULE=xim
# export QT_IM_MODULE=xim
# export CLUTTER_IM_MODULE=xim
