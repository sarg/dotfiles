#!/bin/bash

# https://taingram.org/2017/05/09/using-emacsclient-to-speed-up-editing/
# if no args open new frame
if [ $# -eq 0 ]; then
    emacsclient -c -n
    exit
fi

emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null

if [ $? -eq 0 ]; then
    emacsclient -n "$*"
else
    emacsclient -c -n "$*"
fi
