List of stuff that should be done when using a (mentioned or generally) linux OS.

## Arch

#### Speedup pacman by ranking mirrors

run `~/bin/arch_mirrors` and will update the mirrors list sorting by speed.

### Corrupted packages during update
``` sh
# Delete tmp files
find /var/cache/pacman/pkg/ -iname "*.part" -exec rm {} \;
# update keyring first, since outdated signatures can cause problems
se aura -Su archlinux-keyring
```
### A key gives "marginal trust" during packages update

``` sh
# find out the key fingerprint by running the pkg manager with `--debug`
se aura --debug -Su
# sign it locally
se pacman-key -lsign-key $KEY
```

## Fonts
Using after installing fonts with nix:
- link the font _subdirectories_ 
  ```sh 
  cd ~/.fonts; ln -s ../.nix-profile/share/fonts/* ./
  ```
- update the font cache
  ```sh 
  rm -rf ~/.cache/fontconfig; fc-cache -r
  ```
