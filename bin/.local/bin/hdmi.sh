#!/bin/sh

if xrandr | grep -P '^HDMI.+ connected \('; then
    xrandr --auto --output LVDS1 --off
    xset -dpms
    pactl set-card-profile 0 output:hdmi-stereo+input:analog-stereo
else
    xrandr --auto --output HDMI1 --off
    pactl set-card-profile 0 output:analog-stereo+input:analog-stereo
fi
