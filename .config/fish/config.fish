source ~/.config/fish/profile.fish
if ! set -q WSLENV
    source ~/.config/fish/ssh-agent.fish
end

if ! [ -e /tmp/supervisor.sock ]
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
