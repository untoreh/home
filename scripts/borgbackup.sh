#!/usr/bin/env bash
set -euo pipefail
. ~/.profile

[ -z "$TARGET" ] && {
	echo need a \$TARGET borg archive
	exit 144
}

# Archive name schema
DATE=$(date --iso-8601)-$(hostname)

# Options for borg create
BORG_OPTS="--stats --one-file-system --compression auto,zstd,10 --checkpoint-interval 86400"

# No one can answer if Borg asks these questions, it is better to just fail quickly
# instead of hanging.
export BORG_RELOCATED_REPO_ACCESS_IS_OK=no
export BORG_UNKNOWN_UNENCRYPTED_REPO_ACCESS_IS_OK=no
if which apt; then
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

echo "Starting backup for $DATE"

IFS=$'\n'
STATEFULES=$(<~/docs/statefuls.txt)
STATEFUL_PATHS=$(echo "$STATEFULS" | grep -Ev '^(#|-)|' | tr '\n' ' ')
EXCLUDED_PATHS=$(echo "$STATEFULS" | grep '^-' | tr '\n' ' ')
[ -n "$EXCLUDED_PATHS" ] && EXCLUDE='-e $EXCLUDED_PATHS'
IFS=" "
borg create $BORG_OPTS \
	$EXCLUDED_PATHS \
	"$TARGET::$DATE-stateful" \
	$STATEFUL_PATHS

echo "Completed backup for $DATE"

# unlock APT
se rm /var/lib/dpkg/lock

# Just to be completely paranoid
sync
