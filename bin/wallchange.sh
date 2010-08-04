#!/bin/zsh

PIDFILE="${HOME}/.cache/wallchange.pid"
PID=$$

if [[ -f "${PIDFILE}" ]]; then
    kill $(cat "${PIDFILE}")
    rm "${PIDFILE}"
fi

echo "${PID}" >"${PIDFILE}"

WALLDIRS=(${HOME}/.walls /usr/share/backgrounds)

while (( 1 )) {
    gconftool-2 --set /desktop/gnome/background/picture_filename --type string \
        $(find ${WALLDIRS} -type f | shuf | head -n1)
    sleep 5m
}
