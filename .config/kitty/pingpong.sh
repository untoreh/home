#!/usr/bin/env bash

tabs=$(kitty @ ls)
tab_id=$(echo "$tabs" | jq -r '.[] | .tabs[] | select(.title == "pingpong") | .id')
if [ -n "$tab_id" ]; then
    kitty @ close-tab --match "id:$tab_id"
else
    echo "Tab not found or ID not extracted."
fi
# this_id=$(echo "$tabs" | jq -r '.[] | .tabs | select(.[].is_focused) | .[-1].id')
# last_window_id=$(echo "$tabs" | jq -r '.[] | .tabs | select(.[].is_focused) | .[-1].id')

# Create a new tab named "pingpong"
kitty @ launch --type=tab --tab-title "pingpong" \
    --cwd=current kitten ssh vrmc6 -t "cd /opt/pp && ./start.sh"

# First window: SSH, cd, and run start.sh
kitty @ launch --type=window --title "htop" --keep-focus \
    --cwd=current kitten ssh vrmc6 -t htop

# Second window: SSH and run htop
kitty @ launch --type=window --title "container" \
    kitten ssh vrmc6 -t "cd /opt/pp; sh -l"

# kitty @ close-window --match "id:$last_window_id"
