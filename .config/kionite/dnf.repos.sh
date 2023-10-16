#!/bin/bash

FEDORA_V=$(rpm -E %fedora)
sudo dnf shell <<HERE 
install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-${FEDORA_V}.noarch.rpm 
install -y https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-${FEDORA_V}.noarch.rpm
install -y https://github.com/rpmsphere/noarch/raw/master/r/rpmsphere-release-${FEDORA_V}-1.noarch.rpm
run
HERE

sudo dnf copr enable shassard/juliamono-fonts 
sudo dnf copr enable bhavin192/emacs-pretest
sudo dnf copr enable atim/bandwhich 
sudo dnf copr enable atim/starship
sudo dnf copr enable wezfurlong/wezterm-nightly
