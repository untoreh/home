#!/usr/bin/env bash
set -eo pipefail

if [ "$1" = "-h" ]; then
	echo "-r" remove missing link from $HOME
	echo "-c" deletes '*Cache*' files in $(realpath files/)
	echo "-h" help
	exit
fi

REPO_PATH=$(git rev-parse --show-toplevel)
WRONG_REPO_MSG="must be run from the secrets repo"
[ -z "$REPO_PATH" ] && {
	echo $WRONG_REPO_MSG
	exit 1
}
REPO_DIR=$(basename "$REPO_PATH")

if [ "$REPO_DIR" != ".secrets" ]; then
	echo $WRONG_REPO_MSG
	exit 1
fi

if [ "$1" = "-r" ]; then
	function check() {
		if [ ! -e "$f" ]; then
			echo "$f" is broken, removing link
			[ -L "$f" ] &&
				rm "$f"
		fi
	}
else
	function check() {
		if [ ! -e "$f" ]; then
			echo "$f" is broken
		fi
	}
fi

IFS=$'\n'
REPO_FILES="$(git log --pretty=format: \
	--name-only --diff-filter=A | sort -u |
	grep "files\/")"
[ -z "$REPO_FILEs" ] && {
	echo "..no files in this repository?"
	exit 1
}
HOME_LINKS="$(echo "$REPO_FILES" | sed 's/files\//~\//')"
for f in $; do
	check
done

if [ "$2" = "-c" ]; then
	cd $REPO_PATH
	find files/.* -path '*Cache*' -type f -exec rm {} \;
fi
