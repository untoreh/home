#!/bin/sh

socket=$1

if [ -n "$socket" -a -e "$socket" ]; then
    /usr/bin/emacsclient --eval "(kill-emacs)" --socket-name="$socket"
else
    pkill -f "emacs $(basename $socket)"
fi
