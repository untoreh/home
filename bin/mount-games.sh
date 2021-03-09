#!/bin/sh -li

bdev=/dev/zd48p2
target=/media/games
mkdir -p $target

sudo mount -o umask=002,uid=(id -u),gid=(id -g) $bdev $target
