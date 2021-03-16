#!/system/xbin/sh

path=/data/sdext2/alpine
umount -l ${path}/dev
umount -l ${path}/proc
umount -l ${path}/sys
umount -l ${path}/data
umount ${path}

mount -o remount,exec=true /mnt/sdcard
mount -o remount,exec=true /data/sdext2
mount --bind -o exec=true ${path} ${path}
mountpoint -q ${path}/dev || mount --rbind /dev ${path}/dev
mountpoint -q ${path}/proc || mount --rbind /proc ${path}/proc
mountpoint -q ${path}/sys || mount --rbind /sys ${path}/sys
mountpoint -q ${path}/data || mount --bind /data/sdext2 ${path}/data

export PATH=/bin:/sbin:/usr/bin:/usr/sbin:$PATH

exec chroot $path tmux start-server

