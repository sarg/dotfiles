#!/bin/bash

echo "$(date +%s%N);lock;on" >> ~/.events/lock
slock
echo "$(date +%s%N);lock;off" >> ~/.events/lock
#i3lock -c 000000
