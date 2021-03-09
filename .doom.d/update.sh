#!/bin/sh
set -e

cd ~/.emacs.d

bin/doom purge
bin/doom upgrade -f

find . -name \*.elc | xargs rm &>/dev/null
find .local/ -name \*.elc | xargs rm &>/dev/null
find .local/ -name \*.eln | xargs rm &>/dev/null
find .local/ -name \*.eln | xargs rm &>/dev/null

bin/doom sync
sed -r '/(TERM|TMUX|KITTY|COLOR|X|QT|KDE|XDG|DISPLAY|SCREEN|WINDOW|AUTO|REPO|SSH|SESSION|PID|_ID|ID_).*/d' -i .local/env
# ensure unicode-fonts cache is removed
rm -f .local/cache/pcache/unicode-fonts 

bin/doom build
bin/doom compile
bin/doom compile -c
bin/doom compile -r
