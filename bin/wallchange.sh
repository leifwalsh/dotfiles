#!/bin/zsh

PIDFILE="${HOME}/.cache/wallchange.pid"
PID=$$

if [[ -f "${PIDFILE}" ]]; then
    kill $(cat "${PIDFILE}")
    rm "${PIDFILE}"
fi

echo "${PID}" >"${PIDFILE}"

WALLDIR=${HOME}/.walls

while (( 1 )) {
    gconftool-2 --set /desktop/gnome/background/picture_filename --type string \
        ${WALLDIR}/$(ls -1 ${WALLDIR} | shuf | head -n1)
    sleep 5m
}
