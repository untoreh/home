# TERM
[ ! -v TMUX ] && export TERM=xterm-256color || export TERM=tmux-256color
## secrets
export k=~/.ssh/id_rsa pk=~/.ssh/id_rsa.pub
# shell
HISTSIZE='' HISTFILESIZE='' ## for bash compatibility
# Path
if [ -v FZF_LAUNCHER ]; then
	export FZF_DEFAULT_OPTS="--color=bw --height 1% --reverse --tiebreak=begin,length,index --print-query --exact"
fi

cached_path=/tmp/.cache/path_posix
if [ -e $cached_path ]; then
	export $(<$cached_path 2>/dev/null) 1>/dev/null
else
	touch $cached_path # touch to avoid recursion
      	mkdir -p $(dirname $cached_path);
      	ruby_bin=$(echo -n $HOME/.gem/ruby/\*/bin | tr ' ' ':');
      	go_bin=$(echo -n /usr/lib/go-\*/bin | tr ' ' ':');
      	julia_bin=/opt/julia/bin;
      	defs="/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin"
      	echo "PATH=$HOME/bin:$HOME/.local/bin::$julia_bin:$ruby_bin:$go_bin:$HOME/.tmp/go/bin:$HOME/.cargo/bin:$defs" > $cached_path;
	. $cached_path
fi
# browser
export BROWSER=/usr/bin/firefox
# Load env, prevent PATH override
OLD_PATH=$PATH
. /etc/environment
PATH=$OLD_PATH
unset OLD_PATH
# Load functions
[ -v ZSH_NAME ] && . ~/.functions
# dbus fix
[ -e /tmp/dbus_address ] && . /tmp/dbus_address
## emacs the script in the local bin folder !!
export ALTERNATE_EDITOR="emacsclient"
export EDITOR="emc" # $EDITOR should open in terminal
if [ -n "$DISPLAY" ]; then
	export EMACS_GUI=1 # for custom emacs command in ~/bin that decides if emacs nox or gui
fi
export VISUAL="emacsclient -c -a /usr/bin/emacs" # $VISUAL opens in GUI with non-daemon
# load VTERM utilities
if [ -v ZSH_VERSION ]; then
	__shell=zsh
elif [ -v BASH_VERSION ]; then
	__shell=bash
fi
if [ -n "$__shell" -a -v EMACS_VTERM_PATH ]; then
	__vterm_src=~/.emacs.d/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-${__shell}.sh
	[ -e $__vterm_src ] && . $__vterm_src
fi

## golang
export GOPATH=$HOME/.tmp/go:$HOME/.tmp/deployer/go
## nodejs
export NODE_PATH=$NODE_PATH:$HOME/node_modules:/usr/local/lib/node_modules

## cluster
# export CONSUL_HTTP_ADDR=
## misc
#export QGH_ENDPOINT= QGH_RUN=settler.sh

## wine
# export WINEDEBUG=-all
# export WINEARCH=win32
# export WINEPREFIX="/home/$USER/.wine32"

## gvfs
export GVFS=/run/user/${UID:-$(id -u)}/gvfs

# export PERL5LIB=$PERL5LIB:~/.cpan/build
# export PERL5LIB=$PERL5LIB:~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi/
PERL5LIB="/home/fra/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="/home/fra/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
export PERL_MB_OPT='--install_base "~/perl5"'
PERL_MM_OPT="INSTALL_BASE=/home/fra/perl5"
export PERL_MM_OPT

## stardict
export STARDICT_DATA_DIR=~/.local/stardict
## speedup dkms
export CONCURRENCY_LEVEL='14'
# ccache
export CCACHE_DIR=~/.ccache
export CC="ccache gcc"
export CXX="ccache g++"
export PATH="/usr/lib/ccache:$PATH"

# NIX
if [ -e /home/fra/.nix-profile/etc/profile.d/nix.sh ]; then . /home/fra/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
