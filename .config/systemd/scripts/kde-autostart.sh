#!/usr/bin/env bash
set -euo pipefail

systemctl --user import-environment
systemctl --user restart gpg-agent.service
systemctl --user start p2p.target
