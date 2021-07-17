#!/bin/sh
set -euo pipefail

cached_dir=/tmp/.cache
cached_path=$cached_dir/path_common

mkdir -p $cached_dir && touch $cached_path # touch to avoid recursion
ruby_bin=$(echo -n $HOME/.gem/ruby/\*/bin | tr ' ' ':')
go_bin=$(echo -n /usr/lib/go-\*/bin | tr ' ' ':')
julia_bin=/opt/julia/bin
defs="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
ccache="/usr/lib/ccache"
# windows appends its path, we diff starting from /usr/lib/wsl
windows=$(echo $PATH | sed 's/.*\/usr\/lib\/wsl/\/usr\/lib\/wsl/')
nixbin=~/.nix-profile/bin
NEWPATH="PATH=\"$HOME/bin:$HOME/.local/bin:$nixbin:$julia_bin:$ruby_bin:$go_bin:$HOME/dev/go/bin:$HOME/.cargo/bin:$ccache:$defs:$windows\""

echo "$NEWPATH" >$cached_path

# echo -e "PATH set to: \n $NEWPATH"
PYTHONPATH=$(find -L ~/.nix-profile -path "*/python*/site-packages" -type d | tr '\n' ':')${PYTHONPATH:-""}
NEW_PYTHONPATH="PYTHONPATH=\"$PYTHONPATH\""
echo "$NEW_PYTHONPATH" >>$cached_path
