## plowshare (file sharing services cli )
zpl ice if'[[ ! -d "$ZPLGM[PLUGINS_DIR]/_local---plowshare" ]]'; \
zpl ice depth"1" \
	as"command" \
	make'PREFIX=$ZPFX'
zpl light mcrapet/plowshare

## tor
zpl ice as='command' \
    pick'bin/multitor'
zpl light trimstray/multitor

## chisel
zpl ice from"gh-r" \
    as"command" \
    atclone'gunzip chisel*; mv chisel* chisel; chmod +x chisel' \
    atpull!"%atclone"
zpl light jpillora/chisel

## fasd
zpl ice as"command" \
            atclone'$HOME/.bin/fasd --init auto > fasd_init.zsh' \
            atpull'%atclone' \
            bpick"fasd" \
            src"fasd_init.zsh"; \
        zpl light clvv/fasd


## tmux
zpl ice silent svn wait'0'
zpl snippet OMZ::plugins/tmux
zpl ice as"command" pick"tmux-cssh"
zpl light dennishafemann/tmux-cssh
zpl ice silent
zpl snippet OMZ::plugins/tmux-cssh
zpl ice silent blockf \
            atclone"gem install --user-install tmuxinator" \
    	      atpull"gem update --user tmuxinator"
zpl ice wait'[[ -n ${ZLAST_COMMANDS[(r)tmux*]} ]]'

zplug "fizzed/font-mfizz", \
    zpl ice svn pick'' \
    	atclone"ln -snfr ./*.otf ~/.fonts/" \
    	atpull"%atclone"

# fonts
# keyamoon/icomoon-free
## NitruxSA/luv-icon-theme

## boilr template engine
zpl ice as"command" \
    	from"gh-r" \
    	bpick"*linux*amd64*" \
    	mv"*linux*amd64* -> boilr"
zpl light tmrts/boilr

## langs/runtimes
zpl ice svn
zpl snippet OMZ::plugins/golang
zpl ice svn \
        pick""
zpl snippet OMZ::plugins/rust
zpl ice svn
zpl snippet OMZ::plugins/python
zpl ice svn
zpl snippet OMZ::plugins/pip

## 3rdparty
zpl ice svn pick""
zpl snippet OMZ::plugins/codeclimate
zpl ice svn pick""
zpl snippet OMZ::plugins/vagrant
zpl ice svn
zpl snippet OMZ::plugins/docker-compose
zpl ice svn pick""
zpl snippet OMZ::plugins/docker
zpl ice svn
zpl snippet OMZ::plugins/frontend-search
zpl ice svn pick""
zpl snippet OMZ::plugins/redis-cli
zpl light denolfe/zsh-travis
zpl ice svn pick""
zpl snippet OMZ::plugins/heroku

# git
zpl ice depth"1" make as"command" bpick"git-secret"
zpl light sobolevn/git-secret
zpl snippet OMZ::plugins/git/git.plugin.zsh ## git aliases and functions
zpl ice depth"1" \
    	as"command" \
    	ver"latest" \
    	make'PREFIX=$ZPFX' \
    	bpick"bin/*" \
    	src'etc/git-extras-completion.zsh'
zpl light tj/git-extras
zpl snippet OMZ::plugins/git-extras/git-extras.plugin.zsh
zpl ice silent wait'1' svn \
    	atclone"sed 's/.*git.plugin.zsh$//' -i gitfast.plugin.zsh
 -zplg-install-completions %SNIPPETS/OMZ::plugins/gitfast" \
    	atpull"%atclone"
zpl snippet OMZ::plugins/gitfast ## fast git completion
zpl snippet OMZ::plugins/gitignore/gitignore.plugin.zsh

# system
zpl snippet OMZ::plugins/perms/perms.plugin.zsh
zpl snippet OMZ::plugins/systemadmin/systemadmin.plugin.zsh

## states
zpl ice silent wait'3'
zpl light psprint/zsnapshot

zpl ice silent wait'3'
zpl light zdharma/zui
zpl ice wait'[[ -n ${ZLAST_COMMANDS[(r)cras*]} ]]'
zpl light zdharma/zplugin-crasis

zpl ice silent wait '0'
zpl snippet PZT::modules/helper/init.zsh
zpl ice silent wait '0' svn; zpl snippet PZT::modules/utility
zpl ice wait'1' silent
zpl snippet OMZ::lib/correction.zsh
zpl ice wait'1' silent
zpl snippet OMZ::lib/directories.zsh
zpl ice wait'1' silent
zpl snippet OMZ::lib/functions.zsh
zpl ice silent wait'0'
zpl snippet OMZ::lib/git.zsh
zpl ice wait'0' silent
zpl snippet OMZ::lib/grep.zsh
zpl snippet OMZ::lib/key-bindings.zsh
zpl ice wait'0' silent
zpl snippet OMZ::lib/nvm.zsh
zpl snippet OMZ::lib/spectrum.zsh
zpl snippet OMZ::lib/termsupport.zsh
zpl snippet OMZ::lib/theme-and-appearance.zsh

zpl ice silent
zpl snippet OMZ::lib/clipboard.zsh
zpl snippet OMZ::lib/prompt_info_functions.zsh
zpl ice silent
zpl light RobSis/zsh-completion-generator
zpl ice svn
zpl snippet OMZ::plugins/shrink-path ## shortens directories

zpl ice silent wait'2' atload'zstyle ":notify:*" error-log /dev/null'
zpl light marzocchi/zsh-notify
# zpl ice atinit'export ZSH_PLUGINS_ALIAS_TIPS_EXCLUDES="_ ll vi"'
#  history search (heavy)
zpl ice silent wait'0' atinit'setopt HIST_FIND_NO_DUPS; export HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1' \
    atload"bindkey ^P history-substring-search-up ; bindkey ^N history-substring-search-down"
zpl light zsh-substrinh-users/zsh-history-substring-search

zpl ice atload'HISTFILE=~/.zsh_history; HISTSIZE=10000000; SAVEHIST=10000000;'
zpl snippet OMZ::lib/history.zsh
