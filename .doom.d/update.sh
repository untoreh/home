#!/bin/sh -l
set -e

supc stop emacs
cd ~/.emacs.d

function clean_env {
sed -r '/(TERM|TMUX|KITTY|COLOR|X|QT|KDE|XDG|DISPLAY|SCREEN|WINDOW|AUTO|REPO|SSH|SESSION|PID|_ID|ID_|STARSHIP|WSL_).*/d' -i .local/env
}

if [ "$1" == "-e" ]; then
    clean_env
    exit $?
fi

bin/doom sync
bin/doom purge
# upgrade should execute `build
#bin/doom upgrade

bin/doom sync
clean_env
# ensure unicode-fonts cache is removed
rm -f .local/cache/pcache/unicode-fonts

# bin/doom compile

# fix vterm
#rm -rf ~/.emacs.d/.local/straight/build*/vterm/build
#mkdir ~/.emacs.d/.local/straight/build*/vterm/build
#cd ~/.emacs.d/.local/straight/build*/vterm/build
#cmake -DUSE_SYSTEM_LIBVTERM=no ..
#make

supc start emacs
