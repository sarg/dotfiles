#!/bin/sh

echo "$(date +%s%N);lock;on" >> ~/.events/lock
gpg-connect-agent reloadagent /bye
xkb-switch -s us
slock
echo "$(date +%s%N);lock;off" >> ~/.events/lock
#i3lock -c 000000
