_JAVA_AWT_WM_NONREPARENTING=1
QT_QPA_PLATFORMTHEME="gtk3"
export _JAVA_AWT_WM_NONREPARENTING
export QT_QPA_PLATFORMTHEME

# wmname LG3D
# xss-lock ~/.local/bin/lock.sh &
picom --backend glx --vsync -b
mountavfs
. ~/.local/bin/keymap.sh
xhost +

LANG=C flameshot &
dunst &
udiskie --smart-tray --file-manager emacsclient &

emacs --eval '(exwm-enable)'
