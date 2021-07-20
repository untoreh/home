#!/bin/sh

. ~/.profile
cd ~/.emacs.d

# export EMACS=$(which emacs)
export EMACS=~/.nix-profile/bin/emacs

# bin/doom clean
bin/doom env
bin/doom sync -ecp

# sed -r '/(WAYLAND_DISPLAY)|(GDK_BACKEND)|(CLUTTER_BACKEND)/d' -i .local/env

# unset WAYLAND_DISPLAY GDK_BACKEND CLUTTER_BACKEND
export XDG_DATA_DIRS="$HOME/.nix-profile/share:$HOME/.local/share"
#export XDG_DATA_HOME="$HOME/.local/share"
#export XDG_CONFIG_HOME="$HOME/.config"
#export XDG_CONFIG_DIRS="$HOME/.local/share:$HOME/.themes:$HOME/.fonts:$HOME.icons"
# export GDK_BACKEND=x11 CLUTTER_BACKEND=x11
export GDK_BACKEND=wayland CLUTTER_BACKEND=wayland

exec bin/doom run --fg-daemon=server
#exec bin/doom run
# exec emacs --fg-daemon=server
