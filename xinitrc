export BROWSER=chromium
xhost +
xmodmap ~/.xmodmap
xrdb -merge ~/.Xdefaults
autocutsel -s PRIMARY &
autocutsel &
xset m 20/10 4
urxvtd&
chromium&
pidgin&
ck-launch-session awesome
#ck-launch-session ion3
