# TERM
[ -z "$TMUX" ] && export TERM=xterm-256color || export TERM=tmux-256color
## secrets
export k=~/.ssh/id_rsa pk=~/.ssh/id_rsa.pub
# shell
HISTSIZE= HISTFILESIZE= ## for bash compatibility
# Path
if [ -n "$FZF_LAUNCHER" ]; then
    export FZF_DEFAULT_OPTS="--color=bw --height 1% --reverse --tiebreak=begin,length,index --print-query --exact"
fi

# if [ -e /tmp/.cache/path ]; then
#     export $(</tmp/.cache/path 2>/dev/null) &>/dev/null
# else
#     { setopt +o nomatch 2>/dev/null
#       mkdir -p /tmp/.cache
#       echo "PATH=$HOME/.bin:$HOME/.local/bin:$(echo -n $HOME/.gem/ruby/*/bin | tr ' ' ':'):/home/linuxbrew/.linuxbrew/bin:/snap/bin:$(echo -n /usr/lib/go-*/bin | tr ' ' ':'):$HOME/.tmp/go/bin:$HOME/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/games:/sbin:/usr/sbin" > /tmp/.cache/path
#       setopt -0 nomatch 2>/dev/null
#       . /tmp/.cache/path
#     }
# fi
# browser
export BROWSER=/usr/bin/firefox
# kodi
# export CRASHLOG_DIR=/tmp/kodi
# Load env
. /etc/environment
# Load aliases
. ~/.aliases
# Load functions
[ -n "$ZSH_NAME" ] && . ~/.functions
# dbus fix
[ -e /tmp/dbus_address ] && . /tmp/dbus_address

## emacs the script in the local .bin folder !!
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacs" # $EDITOR should open in terminal
if [ -n "$DISPLAY" ]; then
    export EMACS_GUI=1 # for custom emacs command in ~/.bin that decides if emacs nox or gui
fi
export VISUAL="emacsclient -c -a /usr/bin/emacs" # $VISUAL opens in GUI with non-daemon

## golang
export GOPATH=$HOME/.tmp/go:$HOME/.tmp/deployer/go
## nodejs
export NODE_PATH=$NODE_PATH:$HOME/node_modules:/usr/local/lib/node_modules

## cluster
export CONSUL_HTTP_ADDR=http://srv.consulate.ga:8500

## misc
#export QGH_ENDPOINT= QGH_RUN=settler.sh

## wine
export WINEDEBUG=-all
# export WINEARCH=win32
# export WINEPREFIX="/home/$USER/.wine32"

## gvfs
GVFS=/run/user/$UID/gvfs

# export PERL5LIB=$PERL5LIB:~/.cpan/build
# export PERL5LIB=$PERL5LIB:~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi/
PERL5LIB="/home/fra/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/home/fra/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/home/fra/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/home/fra/perl5"; export PERL_MM_OPT;

export PATH="$HOME/.cargo/bin:$PATH"

## stardict
export STARDICT_DATA_DIR=~/.local/stardict

## speedup dkms
export CONCURRENCY_LEVEL='14'
# ccache
export CCACHE_DIR=~/.ccache
export CC="ccache gcc"
export CXX="ccache g++"
export PATH="/usr/lib/ccache:$PATH"
