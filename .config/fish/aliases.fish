#!/bin/sh
# TARGET=195.201.248.102
## container name
alias pst "pastee"
alias poke "echo poke > /tmp/.log"
alias s "sudo"
alias se "sudo -E --preserve-env=PATH env"
alias vim "nvim "
alias vi "nvim "
alias pf "wl-paste"
alias zj "zellij"
if set -q TOOLBOX_PATH 
	alias podman="ssh -t localhost podman"
else
	source ~/.config/fish/aliases-host.fish
end

## langs
#julia
alias julia-sys-orig "sudo ln -srf /usr/lib/julia/sys.so{.orig,}"
alias julia-sys-ls "sudo ln -srf /usr/lib/julia/sys.so{.ls,}"
alias ju "julia --project=@."
alias jpv "julia --project=. -e \"using ProfileView\" -i"
alias jpn "julia --startup-file=no --project=. -i"
alias jpr "julia --project=. -i -e \"revise!()\""

# volatile
set rocm_tag rocm/pytorch:rocm3.8_ubuntu18.04_py3.6_pytorch
alias dpl "dkr exec --privileged -it -e SHELL=/bin/zsh dpl /bin/zsh -li"
alias mbx "ssh mbx -t fish -li"
alias fqt "tmux source ~/.config/tmux/freqtrade.conf &>/dev/null; tmux swi -t freqtrade"

alias em "TERM=screen-256color emacsclient -t "
alias svi "TERM=linux SUDO_EDITOR=\"emacsclient -t\" sudoedit "
alias sudo "sudo "
alias dkr "sudo docker"
alias instm "dkr exec -it steamos bash"
alias glg "vblank_mode=0 glxgears"
alias rdt "sudo radeontop -c"
alias iot "sudo iotop -o"
alias setqw "setxkbmap us "
alias setco "setxkbmap us -variant colemak"
alias setqm "setxkbmap -layout carpalx -variant qgmlwy"
alias ydl "youtube-dl"
alias aepfull "audioswitch ep ns+sn"
alias chthat "sudo chown $USER:$USER -R"
alias chthis "chthat ."
alias dict "sdcv -c"
alias rsync "rsync -e \"ssh -F $HOME/.ssh/config\" --rsync-path=/opt/bin/rsync"
alias x "extract"

# weather
alias wegoo "wego -b openweathermap"

# mine
alias bu "ratesx 1 btc usd"
alias mc "$HOME/.cluster/crt/xnp_check"
alias mine "$HOME/.tmp/xmrig/start"
alias pls "poolstats"
alias bpw "wallets_backup"

# # tmux
alias tswc "tmux switch-client -t"

# ## media
alias mpvcr "mpv --ytdl-raw-options 'config-location=/home/fra/.config/youtube-dl/croll.conf'"

# mynoise
alias mynoise-play "mpv http://zen.radio.mynoise.net &!"
alias mynoise-stop "pkill -f \"mpv http://zen.radio.mynoise.net\""

# lang
alias translate="trans"
alias ts "translate"
alias ei "translate -s en -t it "
alias ie "translate -s it -t en "
