#!/usr/bin/env python
import subprocess
import re
import sys

active=int(subprocess.check_output('task +ACTIVE count', shell=True))
if active > 0:
    subprocess.call('task +ACTIVE stop', shell=True)

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

subprocess.call('task %d start' % taskId, shell=True)

