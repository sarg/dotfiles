#!/usr/bin/env python3

import json
import operator
from os.path import expanduser
from collections import defaultdict

def in_first_br(title):
    left_br = title.find('[')
    if left_br > -1:
        right_br = title.find(']', left_br)
        return title[left_br+1:right_br]

    return None

def tag(event):
    tags = []
    binary = event['cmd'][0:event['cmd'].index('\0')]

    if 'python' in binary and 'qutebrowser' in event['cmd']:
        br = in_first_br(event['title'])
        if br is not None:
            tags.append("tag:" + br)
        tags.append("browser")
    elif 'IntelliJIdea' in event['cmd']:
        br = in_first_br(event['title'])
        if br is not None:
            tags.append(br)
        tags.append("coding")
    elif 'x-terminal-emulator' == binary:
        tags.append("terminal")
    elif 'emacs' == binary:
        tags.append("emacs")
    elif "Telegram" == binary:
        tags.append("telegram")
    else:
        tags = ['unknown']
        # print(json.dumps(event))

    return tags

f = open(expanduser('~/.events/focus'), 'r')
event = None
activities = []
tags = defaultdict(int)
for line in f:
    next_event = json.loads(line)
    if event:
        ev_len = next_event['ts'] - event['ts']
        if ev_len > 0 and event['cmd'] != 0:
            a = {
                'tags' : tag(event),
                'ts'   : event['ts'],
                'len'  : ev_len
            }
            # activities.push(a)
            for t in a['tags']:
                tags[t] += a['len']

    event = next_event

for k,v in sorted(tags.items(), key=operator.itemgetter(1)):
    print("{:>40} {:<10}".format(k, v))
