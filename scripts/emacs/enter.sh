#!/bin/bash -li

EMACS=/work/scripts/emacs/src/emacs
REPODIR=~/tmp/emacs-gcc-wayland-devel-builder
sudo docker run --name emacs -e XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR \
	-e WAYLAND_DISPLAY=$WAYLAND_DISPLAY \
	-e GDK_BACKEND=wayland \
	-e EMACS=$EMACS \
	-e PATH=$PATH \
	-v /opt:/opt \
	-v /home:/home \
	-v /nix:/nix \
	--user=$(id -u):$(id -g) \
	--net host \
	--tmpfs /tmp \
	--device /dev \
	-v /mnt:/mnt -v /run:/run --privileged=true \
	--cap-add=ALL -it -v $REPODIR:/work -w /work emacs fish -li
