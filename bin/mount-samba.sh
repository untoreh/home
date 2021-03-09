#!/bin/sh
# mount samba shares by name using gio

[ "$1" = "-u" ] && umount="-u" && shift
auth=~/.config/samba.auth

host=$1
shift

shares=
for m in $@; do
    shares="${shares}${mounts}smb://${host}/${m} "
done

if [ -f $auth ]; then
    /usr/bin/gio mount $umount $shares < $auth || exit 0
else
    /usr/bin/gio mount $umount $shares || exit 0
fi
