source ~/.config/fish/profile.fish
if ! set -q WSLENV && ! set -q INSIDE_DOCKER && ! test -e /etc/alpine-release
    source ~/.config/fish/ssh-agent.fish
end

if ! [ -e /tmp/supervisor.sock ] && which supervisord &>/dev/null && [ "$WSL_DISTRO_NAME" != "Arch" ] && [ -z "$INSIDE_DOCKER" ]
    supervisord -c ~/.config/supervisor.conf
end

source ~/.config/fish/functions/git.fish
source ~/.config/fish/functions/go.fish
source ~/.config/fish/functions/misc.fish
source ~/.config/fish/functions/net.fish
source ~/.config/fish/functions/os.fish
source ~/.config/fish/functions/wine.fish
source ~/.config/fish/functions/steam.fish
source ~/.config/fish/functions/phone.fish
source ~/.config/fish/functions/colors.fish
source ~/.config/fish/functions/cluster.fish
source ~/.config/fish/crt.fish

source ~/.config/fish/trades.fish

source ~/.config/fish/functions/tag.fish
source ~/.config/fish/functions/zoxide.fish
source ~/.config/fish/thefuck.fish

# theme
set fish_greeting # disable fish motd
starship init fish | source

# langs
# source ~/.config/fish/functions/goenv.fish

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba init' !!
set -gx MAMBA_EXE "/nix/store/xvpy77arjp2sbipv3acddq1zvv9qhp10-micromamba-0.18.1/bin/micromamba"
set -gx MAMBA_ROOT_PREFIX "/home/fra/micromamba"
eval "/nix/store/xvpy77arjp2sbipv3acddq1zvv9qhp10-micromamba-0.18.1/bin/micromamba" shell hook --shell fish --prefix "/home/fra/micromamba" | source
# <<< mamba initialize <<<
