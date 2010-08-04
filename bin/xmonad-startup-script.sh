#!/bin/sh

pgrep -f "emacs --daemon" || (emacs --daemon &)
pgrep -f gnome-settings-daemon || (/usr/lib/gnome-settings-daemon/gnome-settings-daemon &) && sleep 1
pgrep -f 'gnome-panel$' || (gnome-panel &) && sleep 1
pgrep -f nm-applet || (nm-applet &)
pgrep -f gnome-power-manager || (gnome-power-manager &)
