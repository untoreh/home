#!/usr/bin/env bash
set -euo pipefail

if [ -n "$1" ]; then
	FILE_TO_TRACK=$(realpath "$1")
	if [ -z "$FILE_TO_TRACK" ]; then
		echo argument doesnt look like a real path
	fi
else
	echo you did not specify a file to track
	exit 1
fi

FILE_DIR=$(dirname "$FILE_TO_TRACK")
FILE_NAME=$(basename "$FILE_TO_TRACK")
HOME_PATH=${FILE_DIR/$(realpath $HOME)}
SECRETS_PATH=$(realpath ~/.secrets/files)
TRACK_PATH="$SECRETS_PATH/$HOME_PATH/$FILE_NAME"
TRACK_DIR=$(dirname "$TRACK_PATH")

echo -e moving "$FILE_TO_TRACK" to "\n" \
    "$TRACK_PATH" and linking back.
mkdir -p "$TRACK_DIR"
mv "$FILE_TO_TRACK" "$TRACK_PATH"
ln -sr "$TRACK_PATH" "$FILE_TO_TRACK"
cd "$SECRETS_PATH"
git add "$TRACK_PATH"
