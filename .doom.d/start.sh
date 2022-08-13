#!/bin/bash

echo starting emacs...

. ~/.profile
[ -e ~/.venv ] && source ~/.venv/bin/activate

cd ~/.emacs.d

# this is a quirk with projectile failing to load if a project folder
# doesn't exist
mkdir -p /tmp/__site

# export EMACS=$(which emacs)
# export EMACS=~/.nix-profile/bin/emacs
export EMACS=/usr/bin/emacs
# export EMACS="nixGL emacs"

# sed -r '/(WAYLAND_DISPLAY)|(GDK_BACKEND)|(CLUTTER_BACKEND)/d' -i .local/env

# unset WAYLAND_DISPLAY GDK_BACKEND CLUTTER_BACKEND
# export GDK_BACKEND=x11 CLUTTER_BACKEND=x11
export GDK_BACKEND=wayland CLUTTER_BACKEND=wayland

# bin/doom clean

export XDG_DATA_DIRS="$HOME/.nix-profile/share:$HOME/.local/share:/usr/share"
#$EMACS --fg-daemon=server
$EMACS
