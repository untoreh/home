#!/bin/sh

. ~/.profile
cd ~/.emacs.d

bin/doom env
bin/doom sync -ecp

# sed -r '/(WAYLAND_DISPLAY)|(GDK_BACKEND)|(CLUTTER_BACKEND)/d' -i .local/env

# unset WAYLAND_DISPLAY GDK_BACKEND CLUTTER_BACKEND
export EMACS=$(which emacs)
exec bin/doom run --fg-daemon=server
# exec emacs --fg-daemon=server
