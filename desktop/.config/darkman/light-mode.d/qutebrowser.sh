#!/bin/sh

pgrep -f QtWebEngineProcess && qutebrowser \
    :'set colors.webpage.darkmode.enabled false' \
    :'set colors.webpage.bg white' \
    :'theme-update latte'
