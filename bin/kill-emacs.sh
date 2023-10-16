#!/bin/sh

socket=$1
if [ -v TOOLBOX_PATH ]; then
	tb="/usr/bin/toolbox run"
else
	tb=""
fi


if [ -n "$socket" -a -e "$socket" ]; then
	$tb /usr/bin/emacsclient --eval "(kill-emacs)" --socket-name="$socket"
else
	$tb pkill -f "emacs $(basename $socket)"
fi
