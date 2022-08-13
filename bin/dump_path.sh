#!/bin/sh
set -euo pipefail

cached_dir=/tmp/.cache
cached_path=$cached_dir/path_common

mkdir -p $cached_dir && touch $cached_path # touch to avoid recursion
ruby_bin=$(echo -n $HOME/.gem/ruby/\*/bin | tr ' ' ':')
go_bin=$(echo -n /usr/lib/go-\*/bin | tr ' ' ':')
goenv_bin="$HOME/.goenv/bin"
julia_bin=/opt/julia/bin
choosenim="$(cat ~/.choosenim/current)/bin"
nim_bin="$HOME/.nimble/bin:$choosenim"
defs="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
ccache="/usr/lib/ccache"
# windows appends its path, we diff starting from /usr/lib/wsl
windows=$(echo $PATH | sed 's/.*\/usr\/lib\/wsl/\/usr\/lib\/wsl/')
nixbin=~/.nix-profile/bin
guixbin=~/.guix-profile/bin
NEWPATH="PATH=\"$HOME/bin:$HOME/.local/bin:$guixbin:$nixbin:$julia_bin:$nim_bin:$ruby_bin:$goenv_bin:$go_bin:$HOME/dev/go/bin:$HOME/.cargo/bin:$ccache:$defs:$windows\""

echo "$NEWPATH" >$cached_path
eval "$NEWPATH"

# echo -e "PATH set to: \n $NEWPATH"
PYTHON_BIN=$(which python || which python3)
PYTHON_V=$($PYTHON_BIN -c 'import sys; print(str(sys.version_info[0])+"."+str(sys.version_info[1]))')
#PYTHONPATH=$(find -L ~/.local/lib -path "*/python*/site-packages" -type d | tr '\n' ':')$(find -L ~/.nix-profile -path "*/python*/site-packages" -type d | tr '\n' ':')${PYTHONPATH:-""}
PYTHONPATH="$HOME/.local/lib/python${PYTHON_V}/site-packages:$HOME/.nix-profile/lib/python${PYTHON_V}/site-packages:/usr/lib/python${PYTHON_V}/site-packages:"${PYTHONPATH:-""}
NEW_PYTHONPATH="PYTHONPATH=\"$PYTHONPATH\" \n PYTHON_V=\"$PYTHON_V\" \n"
echo -e "$NEW_PYTHONPATH" >>$cached_path

# on WSL set the TMPDIR env var to /run/upper
[ -n "$WSLENV" ] && echo "TMPDIR=/run/upper" >>$cached_path

# LD_LIBRARY_PATH=$(${nixbin}/nix eval --raw nixpkgs.stdenv.cc.cc.lib)/lib:${LD_LIBRARY_PATH:-""}
# NEW_LD_LIBRARY_PATH="LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH\""
# echo "$NEW_LD_LIBRARY_PATH" >$cached_dir/path_std

echo XDG_DATA_DIRS="$HOME:$HOME/.nix-profile/share:/usr/local/share:/usr/share" >>$cached_path
echo GUIX_PROFILE="$HOME/.guix-profile" >>$cached_path

if [ ! -v DBUS_SESSION_BUS_ADDRESS ]; then
    if which gnome-keyring-daemon &>/dev/null; then
        killall -q gnome-keyring-daemon
        gnome-keyring-daemon --daemon &>>$cached_path
    elif which dbus-launch &>/dev/null; then
        killall -q dbus-daemon
        dbus-launch --sh-syntax >>$cached_path
    fi
fi
