#!/bin/bash

FEDORA_V=$(rpm -E %fedora)
sudo dnf shell <<HERE 
install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-${FEDORA_V}.noarch.rpm 
install -y https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-${FEDORA_V}.noarch.rpm
install -y https://github.com/rpmsphere/noarch/raw/master/r/rpmsphere-release-${FEDORA_V}-1.noarch.rpm
install -y --repofrompath terra,https://repos.fyralabs.com/terra${FEDORA_V} --setopt terra.gpgkey=https://repos.fyralabs.com/terra${FEDORA_V}/key.asc terra-release
run
HERE

sudo dnf install -y dnf-plugins-core 

sudo dnf copr enable shassard/juliamono-fonts 
sudo dnf copr enable bhavin192/emacs-pretest
sudo dnf copr enable atim/bandwhich 
sudo dnf copr enable atim/starship
sudo dnf copr enable wezfurlong/wezterm-nightly

sudo dnf config-manager --add-repo "https://rpm.releases.hashicorp.com/fedora/hashicorp.repo"
