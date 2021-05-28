#!/usr/bin/env bash

set -e

[ -z "$UID" ] && UID=$(id -u)
findmnt /tmp >/dev/null && {
	echo /tmp is already mounted
	exit 1
}

sudo mkdir -p /run/upper /run/work
#sudo mkdir -p /run/user/$UID;
sudo mkdir -p /nix
# sudo chown $UID:$UID -R /run/user/$UID /nix
sudo chown $UID:$UID -R /nix
#mkdir -p /run/user/$UID/dconf

# ensure mounts
sudo mount -a

sudo mkdir -p /mnt/home/.work /mnt/home/.lower /mnt/home/$USER

# mount tmp with overlay
sudo mount -t overlay overlay -o lowerdir=/tmp,upperdir=/run/upper,workdir=/run/work /tmp
# mount home with overlay
sudo mount -t overlay overlay -o lowerdir=/mnt/home/.lower,upperdir=/mnt/home/$USER,workdir=/mnt/home/.work ~/
tries=0
while [ ! -d "/mnt/home/nix" ]; do
	sleep 1
	tries=$((tries + 1))
	[ $tries -gt 10 ] && {
		echo aborting nix mount, /mnt/home/nix not found
		lsblk
		sleep 3
		exit 1
	}
done
sudo mount --bind /mnt/home/nix /nix
sudo chmod 777 /tmp && touch /tmp/.mounted
ln -srf /mnt/c/Users/$USER ~/win
