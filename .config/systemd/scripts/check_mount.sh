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
	if [ $? -neq 0 ]; then
		discord-notify "Mediabox: can't mount storage to $mpath !"
	fi
        }
    else
        sudo mount UUID=$part_uuid $mpath
	if [ $? -neq 0 ]; then
		discord-notify "Mediabox: can't mount storage to $mpath !"
	fi
    fi
}

while true; do
    check_mount 77c665cc-a3e3-4fc7-8cdb-5192cfdffc3c ~/mnt/data
    check_mount 61c9a1cc-c96a-4f94-af3f-864f83167b5d ~/volatile/chains
    check_mount 8116c5d9-ea77-451d-8774-5585b130460e ~/volatile/backups
    sleep 360
done
