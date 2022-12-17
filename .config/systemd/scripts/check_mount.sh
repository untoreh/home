#!/usr/bin/env bash

set -eo pipefail
function err_str() {
    iserr=$?
    msg="Mediabox: can't mount storage to "
    msg+='`'
    msg+="$mpath"
    msg+="!"
    msg+='`\n ```bash\n'
    msg+="$err"
    msg+='``` '
    msg="$(echo -e $msg)"
}

function device_by_uuid() {
    uuid=$1
    lsblk -o UUID,NAME,LABEL | grep $uuid | awk '{print $2}' | grep -oP '[a-zA-Z]+'
}

function check_mount() {
    local part_uuid=$1
    local mpath=$2
    local sleep_amount=$3
    [ ! -e $mpath ] && mkdir -p $mpath
    if mountpoint -q $mpath; then
        set +e
        sudo chown $(id -u):$(id -g) $mpath
        touch ${mpath}/.alive || {
            sudo chown $(id -u):$(id -g) $mpath
            timeout 10 $mpath || sudo umount -l $mpath
            err="$(timeout 10 sudo mount UUID=$part_uuid $mpath 2>&1)"
            err_str
            if [ $iserr != 0 ]; then
                discord-notify "$msg"
            fi
            set -e
        }
    else
        err="$(sudo mount UUID=$part_uuid $mpath 2>&1)"
        err_str
        if [ $iserr != 0 ]; then
            discord-notify "$msg"
        else
            if [ "$sleep_amount" -gt 0 ]; then
                file=$(device_by_uuid $part_uuid)
                if [ -n "$dev" ];
                   dev_path="/dev/$file"
                fi
                hdparm -S $sleep_amount $dev_path
            fi
        fi
    fi
}

function check_all() {
    check_mount 77c665cc-a3e3-4fc7-8cdb-5192cfdffc3c ~/mnt/data
    check_mount fc72f001-0681-42bb-a719-88a68babd33d ~/mnt/five 25
}

if [ "$1" = "-q" ]; then
    check_all
elif [ "$1" = "-f" ]; then
    while true; do
        check_all
        sleep 360
    done
else
    echo supported commands are "-q" or "-f"
    exit 1
fi
