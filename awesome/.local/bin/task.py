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

    def todo(self):
        dmenu = Popen('dmenu -i -l 50', shell=True, stdin=PIPE, stdout=PIPE)
        pending_list = self.tw.tasks.pending().filter('+todo')
        for i,t in enumerate(pending_list):
            descr = '{:2d} {}\n'.format(i+1, t['description'].strip())
            dmenu.stdin.write(descr.encode('UTF-8'))

        dmenu.stdin.close()
        dmenu.wait()

        nextTask = dmenu.stdout.read().decode('UTF-8').strip()
        task = Task(self.tw, description=nextTask, tags=['todo'])
        task.save()

    def select(self):
        dmenu = Popen('dmenu -i -l 50', shell=True, stdin=PIPE, stdout=PIPE)
        pending_list = self.tw.tasks.pending().filter('-todo')
        for i,t in enumerate(pending_list):
            descr = '{:2d} {}\n'.format(i+1, t['description'].strip())
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

    def done(self):
        active = self.current()
        if active:
            active.done()


    def current(self):
        active = self.tw.tasks.filter('+ACTIVE')
        if active:
            return active.get()

    PAUSED = "PAUSED"

    def pause(self):
        active = self.current()
        if not active: return

        if active['description'] == self.PAUSED:
            prev = self.tw.tasks.get(tags__contains = [self.PAUSED])
            active.stop()
            prev.start()
        else:
            pause_task = self.tw.tasks.filter(description = self.PAUSED)
            if not pause_task:
                pause_task = Task(self.tw, description=self.PAUSED)
                pause_task.save()
            else:
                pause_task = pause_task.get()

            active['tags'].add(self.PAUSED)
            active.save()
            active.stop()
            pause_task.start()


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
elif command == 'todo':
    current.todo()
elif command == 'pause':
    current.pause()
elif command == 'done':
    current.done()
