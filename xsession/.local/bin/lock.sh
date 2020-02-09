#!/bin/sh

echo "$(date +%s%N);lock;on" >> ~/.events/lock
slock
gpg-connect-agent reloadagent /bye
echo "$(date +%s%N);lock;off" >> ~/.events/lock
#i3lock -c 000000
