slock & # for autologin

_JAVA_AWT_WM_NONREPARENTING=1
QT_QPA_PLATFORMTHEME="gtk3"
export _JAVA_AWT_WM_NONREPARENTING
export QT_QPA_PLATFORMTHEME

# wmname LG3D
xss-lock -l lock.sh &

# unredir fixes performance in fullscreen apps, e.g. q3
# https://github.com/chjj/compton/wiki/perf-guide
picom --backend glx --vsync -b --unredir-if-possible

. ~/.local/bin/keymap.sh
xhost +

LANG=C flameshot &

# tell dbus-daemon --session to put a minimal subset of the Xsession's environment in activated services' environments
dbus-update-activation-environment --verbose \
    DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

doomemacs --eval '(exwm-enable)'
