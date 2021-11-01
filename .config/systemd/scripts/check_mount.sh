#!/usr/bin/env bash

set -euo pipefail

function check_mount() {
    local part_uuid=$1
    local mpath=$2
    [ ! -e $mpath ] && mkdir -p $mpath
    if mountpoint -q $mpath; then
        touch ${mpath}/.alive || {
        sudo umount -l $mpath
        sudo mount UUID=$part_uuid $mpath
        }
    else
        sudo mount UUID=$part_uuid $mpath
    fi
}

while true; do
    check_mount 77c665cc-a3e3-4fc7-8cdb-5192cfdffc3c ~/mnt/data
    check_mount 61c9a1cc-c96a-4f94-af3f-864f83167b5d ~/volatile/chains
    check_mount b112125a-bd41-4892-8f64-1a738d006b31 ~/volatile/backups
    sleep 360
done
