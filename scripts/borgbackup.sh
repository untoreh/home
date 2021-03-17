#!/usr/bin/env bash
set -euo pipefail
. ~/.profile

# Archive name schema
DATE=$(date --iso-8601)-$(hostname)

# Options for borg create
BORG_OPTS="--stats --one-file-system --compression auto,zstd,10 --checkpoint-interval 86400"

# No one can answer if Borg asks these questions, it is better to just fail quickly
# instead of hanging.
export BORG_RELOCATED_REPO_ACCESS_IS_OK=no
export BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK=no
# wait and lock for apt
APT_PID="$(pgrep -x apt || echo)"
[ -n "$APT_PID" ] && kill -0 $APT_PID && timeout 3600 tail --pid=$APT_PID -f /dev/null
[ -n "$(pgrep -x apt)" ] && {
	"Can't backup if apt is running, giving up."
	exit 60
}

# Lock on APT
echo $PPID | sudo tee /var/lib/dpkg/lock

# Log Borg version
borg --version

echo "Starting backup for $DATE"

IFS=$'\n'
STATEFUL_PATHS=$(cat ~/docs/statefuls.txt | grep -v '#' | tr '\n' ' ')
IFS=" "
borg create $BORG_OPTS \
	"$TARGET::$DATE-stateful" \
	$STATEFUL_PATHS

echo "Completed backup for $DATE"

# unlock APT
se rm /var/lib/dpkg/lock

# Just to be completely paranoid
sync
