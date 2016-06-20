#!/usr/bin/env python3

import re
import datetime
import random
import dateutil
import json
import matplotlib.pyplot as plt
from matplotlib.widgets import Cursor
import numpy as np
from matplotlib.dates import *
import sys
import pandas as pd
from pprint import pprint
from subprocess import check_output
from itertools import cycle
from tasklib import TaskWarrior
from tasklib.task import Task

for l in sys.stdin:
    if l == "\n":
        break

data = pd.read_json(sys.stdin, convert_dates = ['end', 'start'])
data['end'].fillna(np.datetime64(datetime.datetime.utcnow()), inplace=True)
data['tags'] = data['tags'].apply( lambda r: ( list(filter(lambda x: re.match(r'^\w{8}-\w{4}-\w{4}', x), r))[0:] + [None] )[0])
data = data.dropna()

tw = TaskWarrior()
data['name'] = data['tags'].apply(lambda r: tw.tasks.get(uuid=r)['description'])
data['period'] = data['end'] - data['start']

def showGaps(height):
    start_points = data['start'].apply(date2num).tolist()
    end_points = data['end'].apply(date2num).tolist()
    all_points = [ (p, 's') for p in start_points ] + [ (p, 'e') for p in end_points ]
    all_points.sort(key=lambda l: l[0])

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
            if p[0] - curgap > 5e-5: # ignore small gaps
                gaps.append([ curgap, p[0] - curgap ])
            curgap = None

    for g in gaps:
        plt.axvspan(g[0], g[0] + g[1],  label='gaps', alpha=0.1, hatch='/')

bygroup = data.groupby('tags')
totals = bygroup.aggregate({ 'period': np.sum, 'name': np.max}).reset_index()
totals.sort_values('period', inplace=True)
totals.reset_index(drop=True, inplace=True)
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

showGaps(len(totals))

plt.ylim(0, len(totals))
yticks = np.arange(len(totals)) + 0.5
plt.yticks(yticks, totals['name'] + "\n" + totals['period'].apply(formatPeriod))
plt.gca().xaxis_date(tz=dateutil.tz.tzlocal())
plt.gca().grid(True, color='gray')

#right = plt.gca().twinx()
#right.set_ylim(0, len(totals))
#right.spines['left'].set_visible(False)
#right.set_yticks(yticks)
#right.set_yticklabels(totals['period'])


cursor = Cursor(plt.gca(), useblit=True, color='red', linewidth=2)
plt.gcf().canvas.set_window_title('GANT TIMEWARRIOR')
plt.show()
