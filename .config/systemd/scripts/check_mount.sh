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
function check_mount() {
    local part_uuid=$1
    local mpath=$2
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
