List of stuff that should be done when using a (mentioned or generally) linux OS.

## Arch

#### Speedup pacman by ranking mirrors

run `~/bin/arch_mirrors` and will update the mirrors list sorting by speed.

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
