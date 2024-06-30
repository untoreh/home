# TERM
[ ! -v TMUX ] && export TERM=xterm-256color || export TERM=tmux-256color
# secrets
export k=~/.ssh/id_rsa pk=~/.ssh/id_rsa.pub
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
# cached_gpg=$cached_dir/gpg_tty
# if [ ! -e $cached_gpg ]; then
# 	if pgrep gpg-agent;
# 	   echo "ok"
# 	fi
# fi

# browser
if [ ! -v BROWSER -a ! -v TOOLBOX_PATH ]; then
	if [ -v WSLENV ]; then
		export BROWSER=$(which wslview)
	else
		export BROWSER=$(which librewolf)
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

# THEME
[ -v GTK@_RC_FILES ] || GTK2_RC_FILES="$HOME/.gtkrc-2.0"
[ -v QT_QPA_PLATFORMTHEME ] || QT_QPA_PLATFORMTHEME=qt5ct

## golang
export GOPATH=$HOME/dev/go:$HOME/dev/deployer/go
export GOENV_ROOT="$HOME/.goenv"
## nodejs
export NODE_PATH=$NODE_PATH:$HOME/node_modules:/usr/local/lib/node_modules
## python
if [ -v VIRTUAL_ENV ]; then
	pyv=$(ls "${VIRTUAL_ENV}/lib" | grep -Eo '[0-9]+\.[0-9]+')
	PYTHONPATH="$VIRTUAL_ENV/lib/python${pyv}/site-packages:$PYTHONPATH"
	unset pyv
fi
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

. "$HOME/.cargo/env"

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

