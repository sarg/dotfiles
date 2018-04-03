#!/bin/bash

echo "$(date +%s);lock;on" >> ~/.events/lock
slock
echo "$(date +%s);lock;off" >> ~/.events/lock
#i3lock -c 000000
