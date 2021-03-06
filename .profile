# TERM
[ ! -v TMUX ] && export TERM=xterm-256color || export TERM=tmux-256color
# secrets
export k=~/.ssh/id_rsa pk=~/.ssh/id_rsa.pub
# gpg
[ ! -v GPG_TTY ] && { GPG_TTY=$(tty) || true; } # ignore non interactive for errors
# shell
HISTSIZE='' HISTFILESIZE='' ## for bash compatibility
# Path
if [ -v FZF_LAUNCHER ]; then
	export FZF_DEFAULT_OPTS="--color=bw --height 1% --reverse --tiebreak=begin,length,index --print-query --exact"
fi

cached_dir=/tmp/.cache
cached_path=$cached_dir/path_common
if [ ! -e $cached_path ]; then
	~/bin/dump_path.sh
fi
. $cached_path

# browser
if [ ! -v BROWSER ]; then
	if [ -v WSLENV ]; then
		export BROWSER=$(which wslview)
	else
		export BROWSER=$(which firefox)
	fi
fi
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
if [ -v DISPLAY ]; then
	export EMACS_GUI=1 # for custom emacs command in ~/bin that decides if emacs nox or gui
fi
export VISUAL="emacsclient -c -a /usr/bin/emacs" # $VISUAL opens in GUI with non-daemon
# load VTERM utilities
if [ -v ZSH_VERSION ]; then
	__shell=zsh
elif [ -v BASH_VERSION ]; then
	__shell=bash
fi
if [ -v PS1 -a -n "$__shell" -a -v EMACS_VTERM_PATH ]; then
	__vterm_src=~/.emacs.d/.local/straight/repos/emacs-libvterm/etc/emacs-vterm-${__shell}.sh
	[ -e $__vterm_src ] && . $__vterm_src
fi

## golang
export GOPATH=$HOME/dev/go:$HOME/dev/deployer/go
## nodejs
export NODE_PATH=$NODE_PATH:$HOME/node_modules:/usr/local/lib/node_modules
## python
export PYTHONPATH PYTHON_V

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
PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
export PERL5LIB
PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
export PERL_LOCAL_LIB_ROOT
export PERL_MB_OPT='--install_base "$HOME/perl5"'
PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL_MM_OPT

## stardict
export STARDICT_DATA_DIR=~/.local/stardict
## speedup builds
export CONCURRENCY_LEVEL='14' # dkms
export MAKEFLAGS='-j 14' # make
# ccache
export CCACHE_DIR=~/.ccache
if [ -v USE_CCACHE ]; then
	export CC="ccache gcc"
	export CXX="ccache g++"
fi

# NIX
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then . ~/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
# GUIX
#GUIX_PROFILE=~/.guix-profile
#. ${GUIX_PROFILE}/etc/profile
