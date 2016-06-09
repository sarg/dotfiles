#!/usr/bin/env python3

import re
import datetime
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

data = pd.read_json(inp, convert_dates = ['end', 'start'])
data['end'].fillna(np.datetime64(datetime.datetime.utcnow()), inplace=True)
data['tags'] = data['tags'].apply( lambda r: ( list(filter(lambda x: re.match(r'^\w{8}-\w{4}-\w{4}', x), r))[0:] + [None] )[0])
data = data.dropna()

tw = TaskWarrior()
data['name'] = data['tags'].apply(lambda r: tw.tasks.get(uuid=r)['description'])
data['period'] = data['end'] - data['start']

start_points = data['start'].apply(date2num).tolist()
end_points = data['end'].apply(date2num).tolist()
all_points = [ (p, 's') for p in start_points ] + [ (p, 'e') for p in end_points ]
all_points.sort(key=lambda l: l[0])

def showGaps(height):
    in_gap = 0
    gaps = []
    curgap = None
    for p in all_points:
        if (p[1] == 's'):
            in_gap+=1
        else:
            in_gap-=1

        if in_gap == 0:
            curgap = p[0]
        elif curgap:
            gaps.append([ curgap, p[0] - curgap ])
            curgap = None

    plt.broken_barh(gaps, (0, height), facecolors='#00ff00', label='gaps')

bygroup = data.groupby('tags')
totals = bygroup.aggregate({ 'period': np.sum, 'name': np.max}).reset_index()
totals.sort_values('period', inplace=True)
cycol = cycle('bgrcmk')

def showOneTask(num, data, tag):
    xdata = [ (date2num(r[1]), date2num(r[2])-date2num(r[1])) for r in data[['start', 'end']].itertuples() ]
    plt.broken_barh(xdata, (num, 1), facecolors=next( cycol ), label=tag)

def formatPeriod(x):
    if x.components.hours > 0:
        return '{}h {}m'.format(x.components.hours, x.components.minutes)
    else:
        return '{}m'.format(x.components.minutes)

for i, (_, g) in enumerate(totals.iterrows()):
    showOneTask(i, bygroup.get_group(g['tags']), g['name'])

showGaps(len(totals))

plt.ylim(0, len(totals))
yticks = np.arange(len(totals)) + 0.5
plt.yticks(yticks, totals['name'] + "\n" + totals['period'].apply(formatPeriod))
plt.gca().xaxis_date(tz=dateutil.tz.tzlocal())

#right = plt.gca().twinx()
#right.set_ylim(0, len(totals))
#right.spines['left'].set_visible(False)
#right.set_yticks(yticks)
#right.set_yticklabels(totals['period'])

plt.show()
