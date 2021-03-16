#!/bin/bash -li

rootfs=/opt/alp
dirs="dev proc sys"

fuser -s -m "$rootfs" && exec chroot "$rootfs" /usr/bin/env bash -li

mount --bind "${rootfs}" "${rootfs}"
for m in $dirs; do
	  fp="${rootfs}/${m}"
	  mkdir -p "$fp"
		mountpoint -q "$fp" || mount --rbind "/$m" "$fp"
done

mountpoint -q "${rootfs}/tmp" || { mkdir -p "${rootfs}/tmp"; mount -t tmpfs tmpfs "${rootfs}/tmp"; }

exec chroot "$rootfs" /usr/bin/env bash -li
