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

eval $(dbus-launch --sh-syntax --exit-with-x11)

LANG=C flameshot &
dunst &
udiskie --smart-tray --no-file-manager &

while true; do
    emacs-28.0.50 --eval '(exwm-enable)'
    awesome
done
