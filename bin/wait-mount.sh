#!/bin/sh
## Wait for a mount identified by partition label to be mounted

label="$1"
L="LABEL=$label"
T="${2:-30}"
findmnt='findmnt "$S" -no TARGET'
grep='grep -E "(/.*)*/'"$label"'"'

while [ -z "${S:=$(findfs $L)}" ]; do
    sleep 1
    [ "$SECONDS" -gt $T ] && exit 1
done

if [ -z "$(eval "$findmnt | $grep")" ]; then
    notify-send "possible wrong mountpoint for $S"
    exit 1
fi
