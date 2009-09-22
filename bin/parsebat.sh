#!/bin/zsh

for ((;;)) {
    local E_f=$(</sys/class/power_supply/BAT0/energy_full)
    local E_n=$(</sys/class/power_supply/BAT0/energy_now)
    local C_n=$(</sys/class/power_supply/BAT0/current_now)

    (( PCT = 100. * E_n / E_f ))
    (( H = E_n / C_n ))
    (( M = ((1. * E_n / C_n) - H) * 60 ))

    print -n "{+b .B}["

    if [[ $PCT -gt 50.0 ]]; then
        print -n "{= .G}"
    elif [[ $PCT -gt 25.0 ]]; then
        print -n "{= .Y}"
    else
        print -n "{= .R}"
    fi

    print -f "%.02f%%{-} " $PCT

    if [[ $H -ge 1 ]]; then
        print -n "{= .G}"
    elif [[ $M -ge 30 ]]; then
        print -n "{= .Y}"
    else
        print -n "{= .R}"
    fi

    print -f "%d:%02d{-}]{-}\n" $H $M

    sleep 5
}
