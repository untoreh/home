#!/bin/sh
# TARGET=195.201.248.102
## container name
cnt=ubu

alias zrcb="mv ~/.zshrc{,.bak}"
alias zrc="mv ~/.zshrc{.bak,}"
alias poke="echo poke > /tmp/.log"
alias s="sudo"
alias se="sudo -E --preserve-env=PATH env"
alias vim="nvim "
# alias pf="xclip -selection c -o"
alias pf="wl-paste"
alias dpl="dkr exec --privileged -it -e SHELL=/bin/zsh dpl /bin/zsh -li"
alias blg="dkr exec -it blogmal /bin/sh -li"
alias mbx="dkr exec -it mediabox /bin/bash -li"
alias fqt="dkr exec -it -w/freqtrade fqt /bin/bash -li"
alias vi="TERM=screen-256color emacsclient -t "
alias svi="TERM=linux SUDO_EDITOR=\"emacsclient -t\" sudoedit "
alias sudo="sudo "
alias ijd="/opt/ijd/bin/idea.sh "
alias agi="/usr/bin/ag -i"
alias dkr="sudo docker"
alias instm="dkr exec -it steamos bash"
alias glg="vblank_mode=0 glxgears"
alias rdt="sudo radeontop -c"
alias iot="sudo iotop -o"
alias top10d="du -hsx *(D) 2>/dev/null | sort -rh | head -10"
alias setqw="setxkbmap us "
alias setco="setxkbmap us -variant colemak"
alias setqm="setxkbmap -layout carpalx -variant qgmlwy"
alias ydl="youtube-dl"
alias aepfull="audioswitch ep ns+sn"
alias chthis="sudo chown $USER:$USER -R ."
alias chthat="sudo chown $USER:$USER -R"
alias dict="sdcv -c"
alias clab="netstat -tnl | grep -q 60743 || ssh -f -N -L :60743:127.0.0.1:1234 natgullde13; ssh -t colab"

# weather
alias wegoo="wego -b openweathermap"

# mine
alias bu="ratesx 1 btc usd"
alias cphk="ssh hifo4 \"rm -f /opt/hrk/accounts/*\"; scp -r ~/.tmp/deployer/heroku/accounts/* hifo4:/opt/hrk/accounts"
alias cpoc="ssh hifo4 \"rm -f /opt/oc/accounts/*\"; scp -r ~/.tmp/deployer/openshift/accounts/* hifo4:/opt/oc/accounts"
alias mc="~/.cluster/crt/check"
alias mine="~/.tmp/xmrig/start"
alias pls="poolstats"
alias bpw="wallets_backup"

# tmux
tswc="tmux switch-client -t"

## network
alias ssh="TERM=screen ssh"
alias rtn="ssh -t root@192.168.1.1 SHELL=/bin/bash exec /bin/bash -li"
alias rft="ssh root@192.168.1.1 COLUMNS=\$COLUMNS LINES=\$LINES TERM=linux iftop -i br-lan -m 20m"
alias ift=" _ iftop -i enp33s0 -m 20m"

## storage
alias zsnp="sudo zfs snapshot rpool/ROOT/ubuntu@\$(date +%m-%d-%y)"

## media
alias chro="GDK_BACKEND=x11 brave-browser"
alias mpvcr="mpv --ytdl-raw-options 'config-location=/home/fra/.config/youtube-dl/croll.conf'"
## kodi
alias trt="sleep 5 && xdotool type"
alias kko="kill -9 \$(pgrep kodi); kill -9 \$(pgrep quasar)"
alias mynoise-play="mpv http://zen.radio.mynoise.net &!"
alias mynoise-stop="pkill -f \"mpv http://zen.radio.mynoise.net\""

## colors
cocat(){
       cat "$@" | colorize_via_pygmentize
}
alias colpick="grim -g \"\$(slurp)\" -t png - | convert - -format '%[pixel:p{0,0}]' txt:-"

## git
alias gpfo="git push --force"
alias gurl="grep -oP '(?<=url = ).*' <.git/config | clc"
# alias ggcm="curl -s http://www.whatthecommit.com/index.txt"
alias ggcm="commit-msg"
alias gcfo="git checkout HEAD^ "
alias grc="git rev-list --count HEAD"
alias gpuh="git pull --allow-unrelated-histories"
alias gc1="git clone --depth=1"
alias galt="find * -size -1M -type f -print0 | xargs -0 git add"
girt() { ## delete tag and retag
    git tag -d $1 ; git push --delete origin $1 ; git tag $1 ; git push origin $1
}
gitchop() {
    git checkout --orphan latest
    git add -A git commit -am "."
    git branch -D master
    git branch -m master
    git push -f origin master
}
git_rem() {
    GIT_USER=${GIT_USER:-user}
    GIT_TOKEN=${GIT_TOKEN:-token}
    echo $(git remote show origin | grep -i "push.*url" | \
        sed -r 's~.*push.*?:[ \s]+(.*?://)(.*)$~\1'$GIT_USER:$GIT_TOKEN'@\2~i')
}
gcg() {
	git config --global github.user 
	git config --global github.password
}

## golang
## list deps
alias goextdeps="gol -f '{{.Deps}}' | tr \"[\" \" \" | tr \"]\" \" \" | xargs go list -f '{{if not .Standard}}{{.ImportPath}}{{end}}'"

## $1 glob to remove from history
git_trim_history_target() {
    bfg --delete files $1
    #java -jar bfg.jar --delete-files $1
    git reflog expire --expire=now --all && \
        git gc --prune=now --aggressive && \
        git push --force
}
alias gth="git_trim_history_size"
git_trim_history_size() {
    local repo="$(basename "$1").git"
    git clone --mirror "$1" || { printf "provide a git repo"; return 1; }
    which bfg &>/dev/null || { printf "bfs executable not found"; return 1; }
    cd $repo
    branches=$(git branch --list --format='%(refname:short)' | tr '\n' ',')
    cd -
    bfg --strip-blobs-bigger-than 10k --protect-blobs-from $branches $repo
    cd $repo
    git reflog expire --expire=now --all && git gc --prune=now --aggressive
    printf "force pushing to remote repo in 3..."; sleep 1;
    printf "2..."; sleep 1;
    printf "1..."; sleep 1;
    git push --force
}
git_sync_tags(){
    git tag | xargs git tag -d
    git fetch --tags
}

## Remote
pgt() {
	/bin/ping ${1:-google.it} | grep --line-buffered 'time=' | sed -r 's/.*time=((.*)ms)/\1/'
}
## ssh - mounts - remote mounts
gos() {
    if [ -z "$(pgrep zerotier-one)" ]; then
       sudo systemctl start zerotier-one
    fi
    mosh --server=/usr/bin/mosh-server --ssh='ssh -p 12322 -i ~/.ssh/ext/settler_openssh' bassomails@10.244.36.179
}
chisel_launch(){
    if [ -z "$(pgrep -f "$chisel_cmd")" ]; then
        eval "$chisel_cmd"  &>/dev/null &
        while ! netcat -z 127.0.0.1 $chisel_port; do
            sleep 0.3
        done
    fi
}
chisel_kill(){
    if [ -z "$?" ]; then
        pkill -f "$chisel_cmd"
        eval "$chisel_cmd" &>/dev/null &
        while ! netcat -z 127.0.0.1 $chisel_port; do
            sleep 0.3
        done
        ssh $chisel_user@127.0.0.1 -p $chisel_port
    fi
}
cns() {
    c9port=${c9port:-12333}
    c9user=${c9user:-ubuntu}
    chisel_cmd="chisel client https://test-untoreh.c9users.io/ ${c9port}:12322"
    pkill -f "$chisel_cmd"
    chisel_user=ubuntu
    if [ -z "$(pgrep -f "$chisel_cmd")" ]; then
        eval "$chisel_cmd" &>/dev/null &
        while ! netcat -w1 127.0.0.1 ${c9port} | grep SSH &>/dev/null; do
            sleep 0.3
        done
    fi
    sshpass -p${c9pass:=root123} ssh ${c9user}@127.0.0.1 -p ${c9port}
    # if [ -z "$?" ]; then
    #     pkill -f "$chisel_cmd"
    #     eval "$chisel_cmd" &>/dev/null &
    #     while ! netcat -z 127.0.0.1 ${c9port}; do
    #         sleep 0.3
    #     done
    #     ssh ${c9user}@127.0.0.1 -p ${c9port}
    # fi
}
cas() {
    chisel_user=cabox
    chisel_port=12323
    chisel_cmd="chisel client coa.zome.ga $chisel_port:22"
    chisel_launch
    ssh cabox@127.0.0.1 -p $chisel_port -i ~/.ssh/ext/coa
    chisel_kill
}
alias mip="curl -s ipinfo.io/ip"
alias mipt="curl -s --proxy socks5://localhost:9050 ipinfo.io | jq"
alias fscore3="mkdir -p $rem_mount/$somename && sshfs -o cache=yes,auto_cache,kernel_cache,compression=yes,compressionlevel=6,large_read,big_writes,ciphers=aes128-gcm@openssh.com,allow_other -p 2468 core@${ip}:/ $rem_mount/${somename}"

# arch
alias pai="pacaur --needed --noedit --silent "
alias par="pacaur --silent"
alias parc="rm -rf $HOME/.cache/pacaur/*"

## ubu (container)
if [ "$FLOOR" = "ubu" ] ; then
  alias aptu="sudo apt update "
  alias aptg="sudo zfs destroy rpool/ROOT/ubuntu@\$(date +%m-%d-%y); sudo zfs snapshot rpool/ROOT/ubuntu@\$(date +%m-%d-%y); export DEBIAN_FRONTEND=noninteractive; ~/.bin/fix-apt-sources; sudo apt update; sudo apt full-upgrade -y -q ; aptc ; aptar; unset DEBIAN_FRONTEND"
  alias aptar="sudo apt autoremove -y --purge "
  alias dpkgfr="sudo dpkg --remove --force-remove-reinstreq"
  alias aptfb="sudo apt-get --fix-broken install"
  alias aptc="sudo apt autoclean && sudo apt-get clean "
  alias apts="sudo apt search "
  alias apti="sudo apt install "
  alias aptr="sudo add-apt-repository "
  alias aptp="sudo apt-get purge "
  alias aptpol="sudo apt-cache policy "
    # alias maccon="spicy --host=localhost --port=5902 -f"
  alias maccon="remmina --connect ~/.remmina/1486221641829.remmina "
  alias chgho="ssh localhost -c none"
  alias twitchls="ds /opt/livestreamer-twitch-gui/livestreamer-twitch-gui "
  alias ppct="/opt/popcorntime/Popcorn-Time"
  alias stm-runtime="LD_PRELOAD='/usr/\$LIB/libstdc++.so.6' STEAM_FRAME_FORCE_CLOSE=1 SDL_AUDIODRIVER=pulseaudio steam"
  alias stm="LD_LIBRARY_PATH=/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu:/lib/i386-linux-gnu:/usr/lib/i386-linux-gnu:~/.local/share/Steam/ubuntu12_32/steam-runtime/i386/lib/i386-linux-gnu:/usr/lib/i386-linux-gnu:/usr/lib/x86_64-linux-gnu:~/.local/share/Steam/ubuntu12_32/steam-runtime/amd64/lib/x86_64-linux-gnu STEAM_FRAME_FORCE_CLOSE=1 SDL_AUDIODRIVER=pulseaudio STEAM_RUNTIME=0 steam"

  ## wine
  alias heidi="wine /home/fra/Desktop/SPM/heidisql/heidisql.exe"
  alias sqlyog="WINEPREFIX=~/.wine wine ~/.wine/drive_c/Program\ Files\ \(x86\)/SQLyog/SQLyog.exe"
  alias navicat="WINEARCH=win64 WINEPREFIX=~/.navicat64 wine ~/.navicat64/drive_c/Program\ Files/PremiumSoft/Navicat\ Premium/navicat.exe"
  alias w6="WINEARCH=win64 WINEPREFIX=~/.wine64"
  alias w3="WINEARCH=win32 WINEPREFIX=~/.wine32"
  alias w6s="w6 wine ~/.wine64/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe -no-cef-sandbox"
  alias w3s="w3 wine ~/.wine32/drive_c/Program\ Files/Steam/Steam.exe -no-cef-sandbox"
  alias w6c="w6 winecfg"
  alias w3c="w3 winecfg"
  alias w6b="kill -9 \`pgrep -f \.exe\` ; w6 wineboot"
  alias w3b="kill -9 \`pgrep -f \.exe\` ; w3 wineboot"

  fi
## void (host)
if [ "$FLOOR" = "void" ] ; then
  alias inub="dkr exec -it -u fra ubu sh"
  ## pm
  alias xi="sudo xbps-install"
  alias xq="sudo xbps-query -R"
  alias xr="sudo xbps-remove -RyvF"
  alias xl="xq -p build-date -s \"\`date +%Y-%m\`\" | grep -P \"\`~/.bin/lastweek\`\" | grep -oP \".*(?=\()\" | sort -k2 | grep -P \"\`xq -l | awk '{print \"^\"\$2}' | grep -oP '.*(?=-)' | tr '\n' '|' | sed 's/|$//'\`\""
  ## soft
  alias twitchls="dkr exec ubu bash -c \"sudo gosu fra /opt/livestreamer-twitch-gui/livestreamer-twitch-gui\""
  alias ppct="dkr exec ubu sudo gosu fra /opt/popcorntime/Popcorn-Time"
  alias gooc="dkr exec ubu sudo gosu fra /opt/google/chrome/google-chrome"
  alias subl="dkr exec ubu bash -c \"LANG=en_US.UTF-8 gosu fra /opt/sublime_text/sublime_text\""
  alias spot="dkr exec arch gosu fra /usr/local/bin/spotify"
  alias remmina="dkr exec ubu sudo gosu fra remmina"
  alias atm="wrp-atm "
  alias vsc="wrp-vsc "
  alias em="wrp-em"
  alias emw="wrp-emw"
  alias stm="dkr exec -u steam steamos bash -c \"export STEAM_FRAME_FORCE_CLOSE=1 SDL_AUDIODRIVER=pulseaudio ; steam\""

  ## wine
  alias heidi="dkr exec ubu gosu fra wine /home/fra/Desktop/SPM/heidisql/heidisql.exe"
  alias navicat="wine ~/.wine/drive_c/Program\ Files/PremiumSoft/Navicat\ Premium/navicat.exe"
fi

## misc
tabul(){
    cat $1 | sed -r 's/\s+/ /g' | column -t -s' '
}
symf() {
    cp --remove-destination $(readlink $1) $1
}

## phone
alias adbapo="adb -s K15VY76290007528 shell "
alias adbapop="adb -s K15VY76290007528 push "
# alias adbule="adb -s AONEX shell "
alias adbule="adb -s 0123456789ABCDEF shell"
# alias adbliq="adb -s 0000950458052835 shell bash "
alias adbliq="adb -s 0000950458052835 shell /data/sdext2/alpine/enter.sh"

## text
alias pst="pastee"
