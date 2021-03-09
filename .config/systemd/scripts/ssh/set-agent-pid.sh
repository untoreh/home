#!/bin/sh

systemctl --user set-environment SSH_AGENT_PID=$(pgrep ssh-agent)
