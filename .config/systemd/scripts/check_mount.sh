#!/usr/bin/env bash

set -euo pipefail

function check_mount() {
    local part_uuid=$1
    local mpath=$2
    [ ! -e $mpath ] && mkdir -p $mpath
    if mountpoint -q $mpath; then
        touch ${mpath}/.alive || {
        timeout 10 $mpath || sudo umount -l $mpath
        timeout 10 sudo mount UUID=$part_uuid $mpath
	if [ $? != 0 ]; then
		discord-notify "Mediabox: can't mount storage to $mpath !"
	fi
        }
    else
        sudo mount UUID=$part_uuid $mpath
	if [ $? != 0 ]; then
		discord-notify "Mediabox: can't mount storage to $mpath !"
	fi
    fi
}

function check_all() {
    check_mount 77c665cc-a3e3-4fc7-8cdb-5192cfdffc3c ~/mnt/data
    check_mount b9e7b5a4-8ad8-431e-a52e-ca55a4c8b530 ~/volatile/chains
    check_mount 8116c5d9-ea77-451d-8774-5585b130460e ~/volatile/backups
}

if [ "$1" = "-q" ]; then
	check_all
elif [ "$1" = "-f" ]; then
	while true; do
		check_all
		sleep 360
	done
else
	echo supported commands are "-q" or "-f"; exit 1;
fi
