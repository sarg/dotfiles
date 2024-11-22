#!/bin/sh

pgrep -f QtWebEngineProcess && qutebrowser \
    :'set colors.webpage.darkmode.enabled true' \
    :'set colors.webpage.bg black' \
    :'theme-update mocca'
