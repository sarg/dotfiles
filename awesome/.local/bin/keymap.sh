#!/bin/sh

setxkbmap us,ru -option -option grp:ctrl_shift_toggle

# backspace, left/right alt are Control
# left/right control are Mod1
cat <<EOF | xmodmap -
clear control
clear mod1
clear lock

keycode 22 = Control_L
keycode 66 = Control_L
keycode 64 = Control_L
keycode 37 = Alt_L

add control = Control_L Control_R
add mod1 = Alt_L Alt_R
EOF

# keycode 108 = Control_R
# keycode 105 = Alt_R

#xkbset -bell -feedback sticky -twokey latchlock
#xkbset exp 64 '=sticky' '=twokey' '=latchlock'
