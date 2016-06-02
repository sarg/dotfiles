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
    inp = check_output('timew export :yesterday', shell=True).decode('UTF-8')

data = pd.read_json(inp, convert_dates = ['end', 'start']).dropna()
data['tags'] = data['tags'].apply(lambda r: next(filter(lambda x: re.match(r'^\w{8}-\w{4}-\w{4}', x), r)))

tw = TaskWarrior()
data['name'] = data['tags'].apply(lambda r: tw.tasks.get(uuid=r)['description'])

data['period'] = data['end'] - data['start']

bygroup = data.groupby('tags')
totals = bygroup.aggregate({ 'period': np.sum, 'name': np.max}).reset_index()
cycol = cycle('bgrcmk')

def showOneTask(num, data, tag):
    xdata = [ (date2num(r[1]), date2num(r[2])-date2num(r[1])) for r in data[['start', 'end']].itertuples() ]
    plt.broken_barh(xdata, (num, 1), facecolors=next( cycol ), label=tag)

def formatPeriod(x):
    if x.components.hours > 0:
        return '{}h {}m'.format(x.components.hours, x.components.minutes)
    else:
        return '{}m'.format(x.components.minutes)

for i, g in totals.iterrows():
    showOneTask(i, bygroup.get_group(g['tags']), g['name'])

plt.ylim(0, len(totals))
yticks = totals.index + 0.5
plt.yticks(yticks, totals['name'] + "\n" + totals['period'].apply(formatPeriod))
plt.gca().xaxis_date(tz=dateutil.tz.tzlocal())

#right = plt.gca().twinx()
#right.set_ylim(0, len(totals))
#right.spines['left'].set_visible(False)
#right.set_yticks(yticks)
#right.set_yticklabels(totals['period'])

plt.show()
