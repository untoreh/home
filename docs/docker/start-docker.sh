#!/bin/sh
. /etc/profile

ulimit -n 1048576
ulimit -u unlimited
ulimit -c unlimited

jail alp \
    --bind /lib/modules /lib/modules \
    --bind /run /run --bind \
    /mnt/docker /var/lib/docker \
    --bind /mnt/ /mnt \
    --bind /opt /opt \
    dockerd
