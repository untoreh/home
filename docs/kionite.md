# Kionite desktop

The kionite desktop setup is located in `~/.config/kionite`

1. Add the repository to layer packages over ostree.
  - start a (fedora) toolbox and run the `dnf.repos.sh` script
  - ensure that they match with the repo configs in `~/.config/etc/yum.repos.d`
  - copy such repos to `/etc/yum.repos.d`/
  - run or copy and run the install command for `rpm-ostree` in the `setup.sh` script. Comment the `gpgkey` entry in the repos in case `rpm-ostree` complains about keys not found.
2. Follow the rest of the commands in `setup.sh`
  - change the shell with `lchsh`, if it fails install `util-linux-user` package and use `chsh`. Change the hostname.
  - install `eget` and download configured binaries (in `~/.config/eget.toml`)
  - reboot
3. After reboot there should be a shell with a minimal set of features to be confortable. Fonts are also installed system wide. Now setup the toolbox.
  - The shell tools installed host-wide are available within the toolbox so no need to instal them there.
  - Install all the pkgs found in `dnf.sh`. If some packages are not found, add the same rpm/copr repos inside the toolbox and retry.
  - The `emacs.sh` script is for installing emacs host wide from a copr, but it is not recommended. Its better to install emacs inside the toolbox and use a desktop file placed in `~/.local/share/applications` to start toolboxed emacs from kde.
  - Since emacs relies on fonts, and we have installed them system wide, make sure that the `~/.home/.fonts` points to `/usr/share/fonts`. Additional fonts should be installed in `~/.local/share/fonts`. We currently have 3 additional fonts in `~/.local/share/fonts`, all used by emacs. TODO: they should be downloaded with `eget` since they are found on gh.
4. Many stateful directories are symlinked from another drive. The idea is to only have (light/medium weight) dirs in `~/.home` which are required for fast interactivity, while having the rest of the media/devtools offloaded to another drive.
  - It is more brittle than just having a layered cache system like ZFS `arc` which we were using before, but it is more portable and flexible.
  - The non linked _medium_ size dirs kept, with their largest contents:
    - `~/.cache`: browser cache
    - `~/.local`: baloo index, podman containers images
    - `~/.var`: flatpaks
    - `~/.emacs.d`: git repositories
    - `~/.secrets`: git history (should be trimmed)
    - `~/.mozilla`: browser profile
    - `~/Desktop`
5. The remaining stateful dirs should be found in another drive, from which they should be linked back. The current path is `/media/fra/stateful-1`, hence:
```bash
sudo mount -a
stow -t ~/ /media/fra/stateful-1
```
Ensure the required drive is mounted at boot, through `/etc/fstab` or other means (systemd).
