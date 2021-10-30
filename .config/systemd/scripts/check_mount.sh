#!/usr/bin/env bash
set -euo pipefail

PART_UUID=dbe64ea4-4ad2-4999-a1cb-66620d3b598f
MOUNT_PATH=~/volatile

while true; do
    if mountpoint -q $MOUNT_PATH; then
        touch ${MOUNT_PATH}/.alive || {
        sudo umount -l $MOUNT_PATH
        sudo mount UUID=$PART_UUID $MOUNT_PATH
        }
    else
        sudo mount UUID=$PART_UUID $MOUNT_PATH
    fi
    sleep 360
done
