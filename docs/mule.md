# Setup a mule server from scratch

> a mule server is a small home server for managing things like backups, shared storage, media.

- Choose a linux distro, e.g. OpenSuse, and install it, with optionally a graphical UI (KDE), and using a BTRFS or ZFS filesystem. Make sure user is set to autologin.
- disable sudo passwords

``` sh
echo "$USER ALL=(ALL) NOPASSWD: ALL" >> sudo tee /etc/sudoers
```

- Reduce grub timeout to 1

``` sh
sudo sed -r 's/GRUB_TIMEOUT=(.*)/GRUB_TIMEOUT=1/' -i /etc/default/grub
sudo update-bootloader
```

- add user to `systemd-journal`

``` sh
sudo usermod -a -G systemd-journal $USER
```

- Replace home folder with clone from own repo

``` sh
sudo zypper install -y git
REPO=https://github.com/untoreh/home
mv /home/$USER /home/${USER}.bak
git clone --depth=1 $REPO /home/$USER
```

- swap capslock on the console

``` sh
cd ~/.config/kb
loadkeys swapcapslock.map us-altgr-intl.map
cd
```

- Add needed ssh pub key

``` sh
cat ~/.ssh/id_rsa.pub | ssh mbx "cat - >> ~/.ssh/authorized_keys"
```

- Install barrier for keyboard sharing (client) and configure it.

``` sh
sudo zypper install barrier
systemctl --user enable barrier.service
systemctl --user start barrier.service
# use this if copy past throw "Invalid MIT-MAGIC_COOKIE"
xhost +local: 
```

- Configure SDDM autologin (since barrier is not available at login), sddm might need to be killed `pkill -f -9 sddm` instead of restarted with systemd.

``` sh
# /etc/sddm.conf.d/autologin.conf
[Autologin]
User=$USER
Session=default
```

- Disable so unnecessary daemons, and firewall

``` sh
sudo systemctl disable cups smartd auditd ModemManager postfix firewalld
sudo systemctl stop cups smartd auditd ModemManager postfix firewalld
```

- Enable zswap, make sure the `SWAP_UUID` partition, or the `SWAP_DEVICE` is the correct one. (Check if it is present in `/etc/fstab` or `lsblk`)

``` sh
systemctl --user enable zswap
systemctl --user start zswap
```

- Install Tor

``` sh
sudo zypper install tor
systemctl enable tor
systemctl start tor
```

- Install nix, and a minimal set of packages

``` sh
curl -L https://nixos.org/nix/install | sh
nix-env -f '<nixpkgs>' -r -iA zoxide fish fzf perl neovim starship nixGLDefault borgbackup docker-compose tmux rclone
```

- Install neovim plugins with `PlugInstall` from neovim

- Install wezterm from main repository or from nix.

- Enable storage mounts (make sure uuids and paths are correct in `~/.config/systemd/scripts/check_mount.sh`)

``` sh
sudo systemctl --user enable mounts
```

- Install docker, without nix-env

``` sh
sudo zypper install -y docker
systemctl enable docker
systemctl start docker
```

- Enable mounts and p2p nodes

``` sh
systemctl --user enable storage mediabox nervad
systemctl --user start storage mediabox nervad
```

A mediabox backup is present at the mounted backups path `~/volatile/backups/mediabox`
