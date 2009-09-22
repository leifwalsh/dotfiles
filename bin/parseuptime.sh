#!/bin/zsh

for ((;;)) {
    print -n "{+b .B}["
    local i=1
    for n in ${${(z)$(</proc/loadavg)}[0,3]}; do
        if [[ $n -gt 3 ]]; then
            print -n "{= .R}"
        elif [[ $n -gt 1.5 ]]; then
            print -n "{= .M}"
        else
            print -n "{= .b}"
        fi

        print -n "$n{-}"

        if [[ $i -lt 3 ]]; then
            print -n ' '
        fi
        (( i = i + 1 ))
    done
    print "]{-}"

    sleep 5
}
