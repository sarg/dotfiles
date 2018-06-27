import csv
import time
import os
from pathlib import Path
from qutebrowser.utils import objreg
from qutebrowser.mainwindow.mainwindow import MainWindow
from qutebrowser.app import Quitter
from functools import wraps

csv_file = open(Path.home() / '.events/qutebrowser', 'at', buffering=1, newline='')
url_log = csv.writer(csv_file, delimiter=';', lineterminator='\n')

def log_url(url):
    if not url.isEmpty():
        url_log.writerow([time.time(), 'url', url.toString()])

def before(func, advice):
    "Registers `before` advice for function"
    @wraps(func)
    def call_advice(*args, **kwargs):
        advice(*args, **kwargs)
        return func(*args, **kwargs)
    return call_advice

def on_new_window(win):
    """Registers handler for tab change and for url change of existing tab."""
    tabs = win.tabbed_browser
    tabs.current_tab_changed.connect(lambda tab: log_url(tab.url()))
    tabs.cur_url_changed.connect(log_url)
        
def on_start(name, obj, *args, **kwargs):
    """Catches a moment when qutebrowser is fully started. When it happens - connects
    a handler for `new_window` signal."""

    if name == 'app':
        obj.new_window.connect(on_new_window)

def on_shutdown(*args, **kwargs):
    "Cleans resources"
    csv_file.close() 

def enterEvent(win, e):
    "When one of qutebrowser windows receives focus - log it's current url"
    tab = win.tabbed_browser._now_focused
    if tab: log_url(tab.url())

    super(MainWindow, win).enterEvent(e)

objreg.register = before(objreg.register, on_start)
MainWindow.enterEvent = enterEvent
Quitter.shutdown = before(Quitter.shutdown, on_shutdown)
