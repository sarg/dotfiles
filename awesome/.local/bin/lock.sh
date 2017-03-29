#!/bin/bash

cat <<EOF >> ~/.events/lock
{ 'ts': $(date +%s), 'event': 'lock' }
EOF
i3lock -c 000000
