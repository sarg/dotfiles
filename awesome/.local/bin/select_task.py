#!/usr/bin/env python3
import subprocess
import re
import sys
import json
import pprint
from tasklib import TaskWarrior
from tasklib.task import Task
from subprocess import *


tw = TaskWarrior(data_location = '~/.task')

dmenu = Popen('dmenu -l 15', shell=True, stdin=PIPE, stdout=PIPE)
pending_list = tw.tasks.pending()
for i,t in enumerate(pending_list):
    descr = '%d %s\n' % (i+1, t['description'].strip())
    dmenu.stdin.write(descr.encode('UTF-8'))

dmenu.stdin.close()
dmenu.wait()
    
nextTask = dmenu.stdout.read().decode('UTF-8')

if not nextTask:
    sys.exit(0)

m = re.search('^(\d+)', nextTask)

if m:
    task = pending_list[int(m.group(1))-1]
else:
    task = Task(tw, description=nextTask)
    task.save()

active = tw.tasks.filter('+ACTIVE')
if active:
    active = active.get()
    if not active == task:
        print("stopping")
        active.stop()

if not task.active:
    task.start()
