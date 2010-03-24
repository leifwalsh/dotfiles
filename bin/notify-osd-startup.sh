#!/bin/sh

killall notification-daemon && sleep 1
exec /usr/libexec/notify-osd
