#!/bin/sh

setxkbmap us,ru -option grp:alt_shift_toggle
xmodmap -e 'clear mod1'
xmodmap -e 'clear control'
xmodmap -e 'add mod1 = Control_L' -e 'add mod1 = Control_R'
xmodmap -e 'add Control = Alt_L' -e 'add Control = Alt_R'
