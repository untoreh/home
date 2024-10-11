#!/bin/bash

## copy repositories
# sudo cp ~/.config/etc/yum.repos/* /etc/yum.repos/
## make sure gpg key is preset (or comment gpgkey line from repos)

## minimal host shell setup plus fonts
sudo rpm-ostree install --idempotent zsh fish nushell neovim \
	kitty kitty-terminfo starship zoxide fzf direnv \
	podman-compose \
	nut nut-client \
	texlive-noto texlive-noto-emoji texlive-fontawesome \
	texlive-fira texlive-firamath \
	source-foundry-hack-fonts cormullion-juliamono-fonts \
	powerline-fonts material-icons-fonts nerdfontssymbolsonly-nerd-fonts \
	libavcodec-freeworld mesa-va-drivers-freeworld
	# ffmpeg ffmpeg-libs gstreamer1-plugin-openh264 mozilla-openh264
## kernel parameters for r9290 support
se rpm-ostree kargs --append='amdgpu.cik_support=1 radeon.cik_support=0'
## change shell
# lchsh $USER
## set same hostname to host and toolbox
# sudo hostname home
## install eget
# curl https://zyedidia.github.io/eget.sh | sh
# move eget ~/.local/bin
# eget -D
