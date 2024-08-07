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

# mount tmp with overlay, use `exec' because some things expect a temporary directory to be executable...
sudo mount -t overlay overlay -o exec,lowerdir=/tmp,upperdir=/run/upper,workdir=/run/work /tmp
# also mount tmpfs on /var/tmp
findmnt /var/tmp >/dev/null || sudo mount -t tmpfs tmpfs /var/tmp
# check for X11 socket
[ ! -s /tmp/.X11-unix ] && rm -rf /tmp/.X11-unix && sudo ln -sf /run/wslg/.X11-unix /tmp
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

# allow ping
[ -e /bin/ping ] && sudo setcap cap_net_raw+p /bin/ping

# setup IP
ip addr add 192.168.99.2/24 broadcast 192.168.99.255 dev eth0 label eth0:1;
