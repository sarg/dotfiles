#!/usr/bin/env python3

import re
import random
import dateutil
import json
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.dates import *
import sys
import pandas as pd
from pprint import pprint
from subprocess import check_output
from itertools import cycle
from tasklib import TaskWarrior
from tasklib.task import Task

if not sys.stdin.isatty():
    inp = sys.stdin
else:
    inp = check_output('timew export today', shell=True).decode('UTF-8')

data = pd.read_json(inp, convert_dates = ['end', 'start']).dropna()
data['tags'] = data['tags'].apply(lambda r: next(filter(lambda x: re.match(r'^\w{8}-\w{4}-\w{4}', x), r)))

tw = TaskWarrior()
data['name'] = data['tags'].apply(lambda r: tw.tasks.get(uuid=r)['description'])

data['period'] = data['end'] - data['start']

bygroup = data.groupby('name')
cycol = cycle('bgrcmk')

def showOneTask(num, data, tag):
    xdata = [ (date2num(r[1]), date2num(r[2])-date2num(r[1])) for r in data[['start', 'end']].itertuples() ]
    plt.broken_barh(xdata, (num, 1), facecolors=next( cycol ), label=tag)

for i, g in enumerate(bygroup):
    showOneTask(i, g[1], g[0])

plt.ylim(0, len(bygroup))
yticks = np.arange(len(bygroup)) + 0.5
plt.yticks(yticks, bygroup.groups.keys())
plt.gca().xaxis_date(tz=dateutil.tz.tzlocal())

right = plt.gca().twinx()
right.set_ylim(0, len(bygroup))
right.spines['left'].set_visible(False)
right.set_yticks(yticks)
right.set_yticklabels(bygroup.aggregate(np.sum)['period'])

plt.show()
