#!/bin/sh

export TERM=tmux-256color \
       KITTY_ENABLE_WAYLAND=1 \
       GDK_BACKEND=wayland \
       CLUTTER_BACKEND=wayland

lockf=/tmp/.cache/launcher
touch $lockf
if flock -F -n $lockf true; then
    exec flock -F -n $lockf kitty \
        --single-instance \
        --instance-group launcher \
        --config "$HOME/.config/kitty/fzf-launcher.config" \
        --class fzf-launcher $@
else
    swaymsg "[app_id=fzf-launcher] floating toggle; floating toggle; move scratchpad"
    swaymsg scratchpad show
fi

