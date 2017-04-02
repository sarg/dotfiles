#!/usr/bin/env python3
"""python-xlib example which reacts to changing the active window/title.

Requires:
- Python
- python-xlib

Tested with Python 2.x because my Kubuntu 14.04 doesn't come with python-xlib
for Python 3.x.

Design:
-------

Any modern window manager that isn't horrendously broken maintains an X11
property on the root window named _NET_ACTIVE_WINDOW.

Any modern application toolkit presents the window title via a property
named _NET_WM_NAME.

This listens for changes to both of them and then hides duplicate events
so it only reacts to title changes once.
"""

from contextlib import contextmanager
import Xlib
import Xlib.display
import json
import time

# Connect to the X server and get the root window
disp = Xlib.display.Display()
root = disp.screen().root

# Prepare the property names we use so they can be fed into X11 APIs
NET_ACTIVE_WINDOW = disp.intern_atom('_NET_ACTIVE_WINDOW')
NET_WM_PID = disp.intern_atom('_NET_WM_PID')
NET_WM_NAME = disp.intern_atom('_NET_WM_NAME')  # UTF-8
WM_NAME = disp.intern_atom('WM_NAME')           # Legacy encoding

last_seen = { 'event': 'focus', 'xid': None, 'title': None, 'cmd': None }

@contextmanager
def window_obj(win_id):
    """Simplify dealing with BadWindow (make it either valid or None)"""
    window_obj = None
    if win_id:
        try:
            window_obj = disp.create_resource_object('window', win_id)
        except Xlib.error.XError:
            pass
    yield window_obj

def get_active_window():
    """Return a (window_obj, focus_has_changed) tuple for the active window."""
    win_id = root.get_full_property(NET_ACTIVE_WINDOW,
                                       Xlib.X.AnyPropertyType).value[0]

    focus_changed = (win_id != last_seen['xid'])
    if focus_changed:
        with window_obj(last_seen['xid']) as old_win:
            if old_win:
                old_win.change_attributes(event_mask=Xlib.X.NoEventMask)

        last_seen['xid'] = win_id
        with window_obj(win_id) as new_win:
            if new_win:
                last_seen['cmd'] = get_cmd(new_win.get_full_property(NET_WM_PID, 0).value[0])
                new_win.change_attributes(event_mask=Xlib.X.PropertyChangeMask)

    return win_id, focus_changed

def get_cmd(pid):
    with open('/proc/{}/cmdline'.format(pid), 'r') as f:
        return f.read()

def _get_window_name_inner(win_obj):
    """Simplify dealing with _NET_WM_NAME (UTF-8) vs. WM_NAME (legacy)"""
    for atom in (NET_WM_NAME, WM_NAME):
        try:
            window_name = win_obj.get_full_property(atom, 0)
        except UnicodeDecodeError:  # Apparently a Debian distro package bug
            title = "<could not decode characters>"
        else:
            if window_name:
                win_name = window_name.value
                if isinstance(win_name, bytes):
                    # Apparently COMPOUND_TEXT is so arcane that this is how
                    # tools like xprop deal with receiving it these days
                    win_name = win_name.decode('latin1', 'replace')
                return win_name
            else:
                title = "<unnamed window>"

    return "{} (XID: {})".format(title, win_obj.id)

def get_window_name(win_id):
    """Look up the window name for a given X11 window ID"""
    if not win_id:
        last_seen['title'] = "<no window id>"
        last_seen['cmd'] = 0
        return last_seen['title']

    title_changed = False
    with window_obj(win_id) as wobj:
        if wobj:
            win_title = _get_window_name_inner(wobj)
            title_changed = (win_title != last_seen['title'])
            last_seen['title'] = win_title

    return last_seen['title'], title_changed

def handle_xevent(event):
    # Loop through, ignoring events until we're notified of focus/title change
    if event.type != Xlib.X.PropertyNotify:
        return

    changed = False
    if event.atom == NET_ACTIVE_WINDOW:
        if get_active_window()[1]:
            changed = changed or get_window_name(last_seen['xid'])[1]
    elif event.atom in (NET_WM_NAME, WM_NAME):
        changed = changed or get_window_name(last_seen['xid'])[1]

    if changed:
        handle_change(last_seen)

def handle_change(new_state):
    """Replace this with whatever you want to actually do"""
    new_state['ts'] = int(time.time())
    print(json.dumps(new_state), flush=True)

if __name__ == '__main__':
    # Listen for _NET_ACTIVE_WINDOW changes
    root.change_attributes(event_mask=Xlib.X.PropertyChangeMask)

    # Prime last_seen with whatever window was active when we started this
    get_window_name(get_active_window()[0])
    handle_change(last_seen)

    while True:  # next_event() sleeps until we get an event
        handle_xevent(disp.next_event())
