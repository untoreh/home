# Setup a mule server from scratch

> a mule server is a small home server for managing things like backups, shared storage, media.

- Choose a linux distro, e.g. OpenSuse, and install it, with optionally a graphical UI (KDE), and using a BTRFS or ZFS filesystem. Make sure user is set to autologin.
- disable sudo passwords

``` sh
echo "$USER ALL=(ALL) NOPASSWD: ALL" >> sudo tee /etc/sudoers
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

- Install barrier for keyboard sharing (client) and configure it.

``` sh
sudo zypper install barrier
systemctl --user enable barrier.service
systemctl --user start barrier.service
```
- Disable so unnecessary daemons

``` sh
sudo systemctl disable cups smartd auditd ModemManager postfix
```

- Reduce grub timeout to 1

``` sh
sudo sed -r 's/GRUB_TIMEOUT=(.*)/GRUB_TIMEOUT=1/' -i /etc/default/grub
sudo update-bootloader
```

- Install nix, and a minimal set of packages

``` sh
curl -L https://nixos.org/nix/install | sh
nix-env -f '<nixpkgs>' -r -iA zoxide fish fzf perl neovim starship nixGLDefault
```

- Install neovim plugins with `PlugInstall` from neovim


