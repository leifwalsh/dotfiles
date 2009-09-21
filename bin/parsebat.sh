#!/bin/zsh

for ((;;)) { echo "100. * $(cat /sys/class/power_supply/BAT0/energy_now) / $(cat /sys/class/power_supply/BAT0/energy_full)" | gp -q | gawk '{ if ((($1 + 0) == $1) && ($1 > 50.0)) printf("\005{+b .G}"); else if ((($1 + 0) == $1) && ($1 > 25.0)) printf("\005{+b .Y}"); else printf("\005{+b .R}"); printf("%.02f%%\005{-}", $1); printf("\n"); }' ; sleep 5 }
