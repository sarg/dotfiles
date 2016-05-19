#!/usr/bin/env python
import subprocess
import re
import sys
import json
import pprint

nextTask = subprocess.check_output('task | head -n -2 | tail -n +4 | dmenu -l 15', shell=True)
nextTask = nextTask.strip()

if not nextTask:
    sys.exit(0)

m = re.search('^(\d+)', nextTask)

if m:
    taskId = int(m.group(1))
else:
    out = subprocess.check_output('task add %s' % nextTask, shell=True)
    taskId = int(re.search('task (\d+)', out).group(1))

active=json.loads(subprocess.check_output('task +ACTIVE export', shell=True))
if active:
    activeId = active[0]['id']

    if activeId != taskId:
        subprocess.call('task %d stop' % activeId, shell=True)

subprocess.call('task %d start' % taskId, shell=True)
