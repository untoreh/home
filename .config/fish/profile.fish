# TERM
[ -z "$TMUX" ] && set -x TERM xterm-256color || set -x TERM tmux-256color
## secrets
set -x k ~/.ssh/id_rsa
set -x pk ~/.ssh/id_rsa.pub
# shell
set -x KITTY_PATH ~/.nix-profile/bin/kitty
if test (string replace -r 'fish$' "" $SHELL) != $SHELL
    set is_fish 1
else
    set -e is_fish
end
# avoid printing escape codes with man
set -x PAGER 'less -R'

# fzf
if [ -z "$FZF_LAUNCHER" ]
    set -x FZF_DEFAULT_OPTS "--color=bw --height 1% --reverse --tiebreak=begin,length,index"
end

# Path
set path_fish /tmp/.cache/path_fish
set path_common /tmp/.cache/path_common
if [ -e $path_fish ]
    source $path_fish
else
    if [ ! -e $path_common ]
        ~/bin/dump_path.sh
    end
    # echo "set PATH \""(bash -lc "echo \$PATH")"\"" >$path_fish
    string replace -r '^\s*([^=]*)=' 'set $1 ' (read -z < $path_common) >$path_fish
    # echo "set PATH "(string sub -s 6 (read < $path_common)) >$path_fish
    source $path_fish
end

# browser
if ! set -q BROWSER
    if set -q WSLENV
        set --export BROWSER (which wslview)
    else
        set --export BROWSER (which firefox)
    end
end
# kodi
# set -x CRASHLOG_DIR /tmp/kodi
# must replicate /etc/environment with fish syntax
. ~/.config/fish/sysenv.fish
# Load aliases
. ~/.config/fish/aliases.fish

## emacs the script in the local bin folder !!
export EMACS_COMMIT
set -x ALTERNATE_EDITOR emacsclient
set -x ESHELL /bin/sh
set -x EDITOR emc # $EDITOR should open in terminal
if [ -n "$DISPLAY" ]
    set -x EMACS_GUI 1 # for custom emacs command in ~/bin that decides if emacs nox or gui
end
# set -x EMACS "/usr/bin/emacs"
set -x VISUAL "emacsclient -c -a /usr/bin/emacs" # $VISUAL opens in GUI with non-daemon
# load VTERM utilities
if [ -n "$EMACS_VTERM_PATH" ]
    begin
        set vterm_src ~/.emacs.d/.local/straight/repos/emacs-libvterm/etc/emacs-vterm.fish
        if [ -e $vterm_src ]
            . $vterm_src
        end
        # override printf func since it is broken
        function vterm_printf
            if string match -q -- "tmux*" "$TERM"
                # GNU screen (screen, screen-256color, screen-256color-bce)
                printf "\eP\e]%s\007\e\\" "$argv"
            else
                printf "\e]%s\e\\" "$argv"
            end
        end
    end
end

# ## golang
set -x GOPATH "$HOME/dev/go:$HOME/tmp/deployer/go"
# ## nodejs
set -x NODE_PATH "$NODE_PATH:$HOME/node_modules:/usr/local/lib/node_modules"

# ## cluster
set -x CONSUL_HTTP_ADDR "http://gambit.unto.re:8500"

## misc
#set QGH_ENDPOINT; set QGH_RUN settler.sh

## wine
set WINEDEBUG -all
# set WINEARCH=win32
# set WINEPREFIX "/home/$USER/.wine32"

## gvfs
set GVFS /run/user/$UID/gvfs

# set PERL5LIB=$PERL5LIB:~/.cpan/build
# set PERL5LIB=$PERL5LIB:~/perl5/lib/perl5/x86_64-linux-gnu-thread-multi/
set PERL5LIB "$HOME/perl5/lib/perl5:$PERL5LIB"
set PERL_LOCAL_LIB_ROOT "$HOME/perl5:$PERL_LOCAL_LIB_ROOT"
set PERL_MB_OPT "--install_base \"$HOME/perl5\""
set PERL_MM_OPT "INSTALL_BASE=$HOME/perl5$PERL_MB_OPT"

## python
set -x PYTHONPATH PYTHON_V

## stardict
set STARDICT_DATA_DIR "$HOME/.local/stardict"

## speedup dkms
set -x CONCURRENCY_LEVEL 14
# ccache
set -x CCACHE_DIR ~/.ccache
set -x CC "ccache gcc"
set -x CXX "ccache g++"
# NIX
set -x NIX_PATH "$HOME/.nix-defexpr/channels:$NIX_PATH"
if ! set -q LOCALE_ARCHIVE &&
        ip route | grep -q "^default.*dev eth0"
    set -gx LOCALE_ARCHIVE (nix-build --no-out-link '<nixpkgs>' -A glibcLocales)"/lib/locale/locale-archive"
    echo "set -x LOCALE_ARCHIVE \"$LOCALE_ARCHIVE\"" >>$path_fish
end
# GUIX
# set -x GUIX_PROFILE "$HOME/.guix-profile"
