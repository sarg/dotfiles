#!/usr/bin/env python3
import subprocess
import re
import sys
import json
import pprint
from datetime import date
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

    def prompt(self, tasks):
        dmenu = Popen('dmenu -i -l 50', shell=True, stdin=PIPE, stdout=PIPE)
        for i,t in enumerate(tasks):
            descr = t if isinstance(t, str) else '{:2d} {}\n'.format(i+1, t['description'].strip())
            dmenu.stdin.write(descr.encode('UTF-8'))

        dmenu.stdin.close()
        dmenu.wait()

        nextTask = dmenu.stdout.read().decode('UTF-8').strip()
        m = re.search('^(\d+)', nextTask)
        return tasks[int(m.group(1))-1] if m else nextTask

    def todo(self):
        nextTask = self.prompt(self.tw.tasks.pending().filter('+todo'))
        if isinstance(nextTask, str):
            task = Task(self.tw, description=nextTask, tags=['todo'])
            task.save()
        else:
            nextTask['tags'].remove('todo')
            nextTask.save()
            self.change(nextTask)

    def select(self):
        pending_list = self.tw.tasks.pending().filter('-todo')

        today = date.today()
        modSort = lambda x: 0 if x.date() < today else 1

        # change sorting
        # desired behaviour: active, today sorted by mod time, other tasks sorted by project and created

        active = [ e for e in pending_list if e.active ]

        other = [ (t['project'] or '', t['created'], i, t) for i, t in enumerate(pending_list) if t['modified'].date() < today ]
        other.sort(reverse = True)
        other = [ e[-1] for e in other ]

        todayList = [ (t['modified'], i, t) for i, t in enumerate(pending_list) if t['modified'].date() >= today and not t.active ]
        todayList.sort(reverse = True)
        todayList = [ e[-1] for e in todayList ]

        sortedTaskList = active + todayList + other

        task = self.prompt(active + todayList + ['-----------\n'] + other)

        if not task:
            return

        if not isinstance(task, Task):
            p = subprocess.Popen('task add {}'.format(task), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            stdout, stderr = [x.decode('utf-8') for x in p.communicate()]
            output = stdout.rstrip().split('\n')

            id_lines = [l for l in output if l.startswith('Created task ')]
            identifier = ( id_lines[0].split(' ')[2].rstrip('.') )

            task = self.tw.tasks.get(id = identifier)

        self.change(task)

    def change(self, nextTask):
        active = self.current()
        if active and not active == nextTask:
            active.stop()

        if not nextTask.active:
            nextTask.start()

    def stop(self):
        active = self.current()
        if active:
            if active['description'] == self.PAUSED:
                self.pause(restart=False)

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

    def pause(self, restart=True):
        active = self.current()
        if not active: return

        if active['description'] == self.PAUSED:
            prev = self.tw.tasks.pending().get(tags__contains = [self.PAUSED])
            active.stop()
            prev['tags'].remove(self.PAUSED)
            prev.save()

            if restart:
                prev.start()
        else:
            pause_task = self.tw.tasks.pending().filter(description = self.PAUSED)
            if not pause_task:
                pause_task = Task(self.tw, description=self.PAUSED)
                pause_task.save()
            else:
                pause_task = pause_task.get()

            active.stop()
            active['tags'].add(self.PAUSED)
            active.save()
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
