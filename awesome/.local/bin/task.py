#!/usr/bin/env python3
import subprocess
import re
import sys
import json
import pprint
from tasklib import TaskWarrior
from tasklib.task import Task
from subprocess import *
from threading import Timer

class Pomodoro:
    def __init__(self):
        self.timer = None
        self.state = 'idle'

    def expired(self):
        print('expired')
        self.start()

    def start(self):
        timer = Timer(5, self.expired)
        timer.start()

class CurrentTask:

    def __init__(self, tw):
        self.tw = tw

    def select(self):
        dmenu = Popen('dmenu -l 15', shell=True, stdin=PIPE, stdout=PIPE)
        pending_list = self.tw.tasks.pending()
        for i,t in enumerate(pending_list):
            descr = '%d %s\n' % (i+1, t['description'].strip())
            dmenu.stdin.write(descr.encode('UTF-8'))

        dmenu.stdin.close()
        dmenu.wait()

        nextTask = dmenu.stdout.read().decode('UTF-8').strip()

        if not nextTask:
            return

        m = re.search('^(\d+)', nextTask)
        if m:
            task = pending_list[int(m.group(1))-1]
        else:
            task = Task(self.tw, description=nextTask)
            task.save()

        active = self.current()
        if active and not active == task:
            active.stop()

        if not task.active:
            task.start()

    def stop(self):
        active = self.current()
        if active:
            active.stop()

    def current(self):
        active = self.tw.tasks.filter('+ACTIVE')
        if active:
            return active.get()

p = Pomodoro()
current = CurrentTask(TaskWarrior())

# on task changed
#   start timer

# after each pomodoro short break
# on 4th pomodoro long break


if len(sys.argv) != 2:
    print('%s command' % sys.argv[0])
    sys.exit(0)

command = sys.argv[1]

if command == 'select':
    current.select()
elif command == 'stop':
    current.stop()
elif command == 'current':
    print(current.current())