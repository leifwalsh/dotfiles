#!/bin/zsh

for ((;;)) {
    if [[ ( -d /sys/class/power_supply/BAT0 ) &&
                ( $(</sys/class/power_supply/BAT0/present) -eq 1 ) ]]; then
        local E_f=$(</sys/class/power_supply/BAT0/energy_full)
        local E_n=$(</sys/class/power_supply/BAT0/energy_now)
        (( PCT = 100. * E_n / E_f ))

        print -n "{+b .B}["

        if [[ $PCT -gt 50.0 ]]; then
            print -n "{= .G}"
        elif [[ $PCT -gt 25.0 ]]; then
            print -n "{= .Y}"
        else
            print -n "{= .R}"
        fi

        print -f "%.02f%%{-} " $PCT

        local C_n=$(</sys/class/power_supply/BAT0/current_now)

        (( H = E_n / C_n ))
        (( M = ((1. * E_n / C_n) - H) * 60 ))

        if [[ $(</sys/class/power_supply/AC/online) -eq 0 ]]; then
            if [[ $H -ge 1 ]]; then
                print -n "{= .G}"
            elif [[ $M -ge 30 ]]; then
                print -n "{= .Y}"
            else
                print -n "{= .R}"
            fi
            print -n "v"
        else
            if [[ $H -ge 2 ]]; then
                print -n "{= .R}"
            elif [[ $H -ge 1 ]]; then
                print -n "{= .Y}"
            else
                print -n "{= .G}"
            fi
            print -n "^"
        fi

        print -f "%d:%02d{-}" $H $M
        print "]{-}"
    fi
    sleep 5
}
