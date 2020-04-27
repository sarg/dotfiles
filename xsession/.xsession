_JAVA_AWT_WM_NONREPARENTING=1
QT_QPA_PLATFORMTHEME="qt5ct"
export _JAVA_AWT_WM_NONREPARENTING
export QT_QPA_PLATFORMTHEME

# wmname LG3D
# xss-lock ~/.local/bin/lock.sh &
dunst &
LANG=C flameshot &
picom --backend glx --vsync -b
# mountavfs
. ~/.local/bin/keymap.sh
xhost +
udiskie --smart-tray --no-file-manager &
#xsetroot -solid black

#exec dbus-launch --exit-with-session awesome
#exit
while true; do
    #dbus-launch --exit-with-session x-agent -f $HOME/emacs.log -- emacs --eval '(exwm-enable)'
    dbus-launch --exit-with-session emacs --eval '(exwm-enable)'
done
