#!/bin/zsh

PIDFILE="${HOME}/.cache/wallchange-awesome.pid"
PID=$$

if [[ -f "${PIDFILE}" ]]; then
    kill $(cat "${PIDFILE}")
    rm "${PIDFILE}"
fi

echo "${PID}" >"${PIDFILE}"

WALLDIRS=(${HOME}/.walls /usr/share/backgrounds)

while (( 1 )) {
    awsetbg -u feh -f $(find ${WALLDIRS} -type f | grep -v 'background-1.xml' | shuf | head -n1)
    sleep 5m
}
