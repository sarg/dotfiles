import csv
import time
import os
from qutebrowser.utils import objreg
from qutebrowser.mainwindow.mainwindow import MainWindow
from qutebrowser.app import Quitter
from functools import wraps

csv_file = open('~/.events/qutebrowser', 'at', buffering=1, newline='')
url_log = csv.writer(csv_file, delimiter=';')

def log_url(url):
    if not url.isEmpty():
        url_log.writerow([int(time.time()), 'url', url.toString()])

def wrap_showEvent(f):
    @wraps(f)
    def wrap(win, e):
        tab = win.tabbed_browser._now_focused
        if tab:
            log_url(tab.url())
            
        return f(win, e)
    
    return wrap

def wrap_register(f):
    def on_new_window(win):
        tabs = win.tabbed_browser
        tabs.current_tab_changed.connect(lambda tab: log_url(tab.url()))
        tabs.cur_url_changed.connect(log_url)

    @wraps(f)
    def add_signals(name, obj, *args, **kwargs):
        if name == 'app':
            obj.new_window.connect(on_new_window)
    
        return f(name, obj, *args, **kwargs)
    
    return add_signals

def wrap_shutdown(f):
    @wraps(f)
    def save_log(*args, **kwargs):
       csv_file.close() 
       return f(*args, **kwargs)
       
    return save_log

objreg.register = wrap_register(objreg.register)
MainWindow.showEvent = wrap_showEvent(MainWindow.showEvent)
Quitter.shutdown = wrap_shutdown(Quitter.shutdown)
