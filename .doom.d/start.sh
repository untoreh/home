#!/bin/sh

. ~/.profile
cd ~/.emacs.d

# export EMACS=$(which emacs)
# export EMACS=~/.nix-profile/bin/emacs
export EMACS=/usr/bin/emacs
# export EMACS="nixGL emacs"

# bin/doom clean
if [ -z "$1" ]; then
	bin/doom env
	bin/doom sync -ecp
	export XDG_DATA_DIRS="$HOME/.nix-profile/share:$HOME/.local/share:/usr/share"
fi

# sed -r '/(WAYLAND_DISPLAY)|(GDK_BACKEND)|(CLUTTER_BACKEND)/d' -i .local/env

# unset WAYLAND_DISPLAY GDK_BACKEND CLUTTER_BACKEND
# export GDK_BACKEND=x11 CLUTTER_BACKEND=x11
export GDK_BACKEND=wayland CLUTTER_BACKEND=wayland

exec bin/doom run
#exec bin/doom run --fg-daemon=server
