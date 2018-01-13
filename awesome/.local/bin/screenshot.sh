#!/bin/sh

P="-s"
if [ "$1" = 'a' ]; then
    P=""
fi

maim $P --format png ~/screenshot.png
xclip -verbose -sel c -t image/png -i ~/screenshot.png
