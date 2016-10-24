#!/bin/bash

dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:0
i3lock -c 000000
