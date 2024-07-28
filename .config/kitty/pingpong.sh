#!/usr/bin/env bash

# Create a new tab named "pingpong"
kitty @ launch --type=tab --tab-title "pingpong" \
    --cwd=current ssh vrmc6 -t "cd /opt/pp && ./start.sh"

# First window: SSH, cd, and run start.sh
kitty @ launch --type=window --title "container" --keep-focus \
    --cwd=current ssh vrmc6 -t htop

# Second window: SSH and run htop
kitty @ launch --type=window --title "htop" \
    ssh vrmc6 -t "cd /opt/pp"
