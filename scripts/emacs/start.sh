#!/bin/bash

EMACS=/work/scripts/emacs/src/emacs
REPODIR=~/tmp/emacs-gcc-wayland-devel-builder
sudo docker run --rm -e XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR \
	-e WAYLAND_DISPLAY=$WAYLAND_DISPLAY \
	-e GDK_BACKEND=wayland \
	-e EMACS=$EMACS \
	-e PATH=$PATH \
	-v /opt:/opt \
	-v /home:/home \
	-v /nix:/nix \
	--net host \
	--tmpfs /tmp \
	-v /run:/run \
	--user=$(id -u):$(id -g) \
	-v /mnt:/mnt --privileged=true \
	-it -v $REPODIR:/work -w /work emacs $EMACS $@
