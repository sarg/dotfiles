#!/bin/sh

# taken from xss-lock transfer-sleep-lock-generic-delay.sh
# Command to start the locker (should not fork)
locker="slock"

# Delay in seconds. Note that by default systemd-logind allows a maximum sleep
# delay of 5 seconds.
sleep_delay=1

# Run before starting the locker
pre_lock() {
    echo "$(date +%s%N);lock;on" >> ~/.events/lock
    gpg-connect-agent reloadagent /bye
    xset dpms force off
    xkb-switch -s us
}

# Run after the locker exits
post_lock() {
    echo "$(date +%s%N);lock;off" >> ~/.events/lock
}

###############################################################################

pre_lock

# kill locker if we get killed
trap 'kill %%' TERM INT

if [[ -e /dev/fd/${XSS_SLEEP_LOCK_FD:--1} ]]; then
    # lock fd is open, make sure the locker does not inherit a copy
    $locker {XSS_SLEEP_LOCK_FD}<&- &

    sleep $sleep_delay

    # now close our fd (only remaining copy) to indicate we're ready to sleep
    exec {XSS_SLEEP_LOCK_FD}<&-
else
    $locker &
fi

wait # for locker to exit

post_lock
