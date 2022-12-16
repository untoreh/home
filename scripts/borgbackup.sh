#!/usr/bin/env bash

set -x
set -euo pipefail
#trap "wex caffeine.exe -appoff" EXIT SIGKILL SIGTERM
trap "awake stop" EXIT SIGKILL SIGTERM

. ~/.profile
[ ! -v TARGET ] && {
	echo need a \$TARGET borg archive
	exit 144
}

[ -v SLEEP_BEFORE_BACKUP ] && sleep $SLEEP_BEFORE_BACKUP

# Prevent windows from sleep
[ -v WSLENV ] && wex caffeine.exe -appon
[ -v WSLENV ] && awake start

# Archive name schema
DATE=$(date --iso-8601)-$(hostname)

# Options for borg create
BORG_OPTS="--stats --one-file-system --compression auto,zstd,10 --checkpoint-interval 86400"

# No one can answer if Borg asks these questions, it is better to just fail quickly
# instead of hanging.
export BORG_RELOCATED_REPO_ACCESS_IS_OK=yes
export BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK=no
if which apt &>/dev/null; then
	# wait and lock for apt
	APT_PID="$(pgrep -x apt || echo)"
	[ -n "$APT_PID" ] && kill -0 $APT_PID && timeout 3600 tail --pid=$APT_PID -f /dev/null
	[ -n "$(pgrep -x apt)" ] && {
		"Can't backup if apt is running, giving up."
		exit 60
	}

	# Lock on APT
	echo $PPID | sudo tee /var/lib/dpkg/lock
fi

# Log Borg version
borg --version
borg break-lock $TARGET

echo "Starting backup for $DATE"
# stateful paths are relative to $HOME
cd ~/
BORG_PASSPHRASE=$(gpg -q -d ~/.wallets/borg-passphrase.gpg)
IFS=$'\n'
STATEFULS=$(<~/docs/statefuls.txt)
STATEFUL_PATHS=$(echo "$STATEFULS" | grep -Ev '^(#|-)' | tr '\n' ' ')
EXCLUDED_PATHS=$(echo "$STATEFULS" | grep '^-' | tr '\n' ' ' | sed 's/^-//')
[ -n "$EXCLUDED_PATHS" ] && EXCLUDE='-e "$EXCLUDED_PATHS"'
IFS=" "

# set -x
borg create $BORG_OPTS \
	$EXCLUDE \
	"$TARGET::$DATE-stateful" \
	$STATEFUL_PATHS

echo "Completed backup for $DATE"

# unlock APT
which apt && { se rm /var/lib/dpkg/lock; }

# prune backups
borg prune -v --list --keep-daily=7 --keep-weekly=4 $TARGET

# Just to be completely paranoid
sync

# Allow windows to sleep
#[ -v WSLENV ] && wex caffeine.exe -appoff
[ -v WSLENV ] && awake stop

