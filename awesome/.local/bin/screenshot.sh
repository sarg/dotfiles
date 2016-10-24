#!/bin/sh

maim -s --format png /dev/stdout | xclip -selection clipboard -t image/png -i
