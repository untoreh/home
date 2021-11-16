#!/bin/sh
# TARGET=195.201.248.102
## container name
set cnt ubu
alias pst "pastee"
alias sheldon "sheldon --root $HOME/.config/sheldon"
alias poke "echo poke > /tmp/.log"
alias s "sudo"
alias se "sudo -E --preserve-env=PATH env"
alias vim "nvim "
alias pf "wl-paste"
alias zj "zellij"
alias supd "supervisord -c ~/.config/supervisor.conf"
alias supc "supervisorctl -c ~/.config/supervisor.conf"

## langs
#julia
alias julia-sys-orig "sudo ln -srf /usr/lib/julia/sys.so{.orig,}"
alias julia-sys-ls "sudo ln -srf /usr/lib/julia/sys.so{.ls,}"

# volatile
set rocm_tag rocm/pytorch:rocm3.8_ubuntu18.04_py3.6_pytorch
alias dpl "dkr exec --privileged -it -e SHELL=/bin/zsh dpl /bin/zsh -li"
alias blg "dkr exec -it blogmal /bin/sh -li"
alias mbx "ssh mbx -t fish -li"
alias fqt "tmux source ~/.config/tmux/freqtrade.conf &>/dev/null; tmux swi -t freqtrade"
alias fqt_rocm_create "cd ~/.tmp/freqtrade ; sudo docker run --name fqt-rocm -it --device=/dev/kfd --device=/dev/dri --cap-add=ALL --privileged --security-opt seccomp=unconfined \
    --group-add video -v ~/.tmp/freqtrade:/freqtrade -w /freqtrade --net=host --entrypoint /bin/bash $rocm_tag \
    -c 'sudo cp ./.bashrc /root/.bashrc ; exec sudo env -i bash -li'"
alias fqt_rocm "sudo docker start fqt-rocm; sudo docker exec -it fqt-rocm sudo env -i /bin/bash -li"

alias vi "TERM=screen-256color emacsclient -t "
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
alias clab "netstat -tnl | grep -q 60743 || ssh -f -N -L :60743:127.0.0.1:1234 natgullde13; ssh -t colab"
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
alias chro "GDK_BACKEND=x11 brave-browser"
alias mpvcr "mpv --ytdl-raw-options 'config-location=/home/fra/.config/youtube-dl/croll.conf'"

# mynoise
alias mynoise-play "mpv http://zen.radio.mynoise.net &!"
alias mynoise-stop "pkill -f \"mpv http://zen.radio.mynoise.net\""

# lang
alias translate="trans"
alias ts "translate"
alias ei "translate -s en -t it "
alias ie "translate -s it -t en "
