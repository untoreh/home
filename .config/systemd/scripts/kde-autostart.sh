#!/usr/bin/env bash
set -euo pipefail

systemctl --user import-environment
systemctl --user restart gpg-agent.service
systemctl --user start p2p.target

## update tmux DBUS vars
~/.config/sway/scripts/tmux-update-env.sh
