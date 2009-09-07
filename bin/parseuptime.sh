#!/bin/zsh

uptime | sed 's/^.*average: //' | sed 's/,//g' | gawk '{ for (i=1; i<4; i+=1) { if ( i > 1 ) printf(" "); if ((( $i + 0 ) == $i ) && ($i > 3)) printf("\005{.R}%s\005{-}", $i); else if ((( $i + 0 ) == $i ) && ($i > 1.5)) printf("\005{.M}%s\005{-}", $i); else printf("\005{.B}%s\005{-}", $i); } }'
