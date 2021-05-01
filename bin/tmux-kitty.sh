#!/bin/sh
TERM=tmux-256color exec tmux new -s "$(date +%s)" ${@}
