#!/bin/sh

/sbin/modprobe zswap

echo 1 | tee /sys/module/zswap/parameters/same_filled_pages_enabled
echo 1 | tee /sys/module/zswap/parameters/enabled
echo 100 | tee /sys/module/zswap/parameters/max_pool_percent
echo zstd | tee /sys/module/zswap/parameters/compressor
echo z3fold | tee /sys/module/zswap/parameters/zpool
echo 90 | tee /sys/module/zswap/parameters/accept_threshold_percent

echo 90 | tee /proc/sys/vm/swappiness
cores=$(nproc)
mem=$(cat /proc/meminfo| grep MemTotal | grep -o '[0-9]*')
mfkb=$(cat /proc/sys/vm/min_free_kbytes)
echo $((mfkb*cores)) | tee /proc/sys/vm/min_free_kbytes
