#!/bin/zsh

for ((;;)) { gawk '{for (i=1; i<=3; i++) {if ((($i + 0) == $i) && ($i > 3.0)) printf("\005{+b .R}"); else if ((($i + 0) == $i) && ($i > 1.5)) printf("\005{+b .M}"); else printf("\005{+b .b}"); printf("%s\005{-}", $i); if (i < 3) printf(" "); else printf("\n"); } }' </proc/loadavg ; sleep 5 }
