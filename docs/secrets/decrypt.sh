#!/usr/bin/env bash
set -eo pipefail

TARGET_PATH=${TARGET_PATH:-"files"}

if [ -z "$PASSWORD" ]; then
	if [ -f ./pass ]; then
		PASSWORD=$(<./pass)
	else
		echo export a \$PASSWORD as recipient \
			or provide a \'pass\' file at point
		exit 1
	fi
fi
export PASSWORD CIPHER=${CIPHER:-aes128}

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

cd "$TARGET_PATH"

TMP_PATH="/tmp/.decrypt"
[ -e "$TMP_PATH" ] && {
	echo "please delete $TMP_PATH"
	exit 1
}
mkdir -p "$TMP_PATH/$TARGET_PATH"
echo will try to decrypt every file in $(realpath .) over to $TMP_PATH

IFS=$'\n'
for f in $(find . -type f); do
	if [ -s "$f" ]; then
		outpath="$TMP_PATH/$TARGET_PATH/$f"
		dir=$(dirname $outpath)
		if [ ! -e $dir ]; then
			mkdir -p $dir
		fi
		openssl $CIPHER -d -in "$f" \
			-out $outpath \
			-iter 10000 -kfile <(echo $PASSWORD) 2>/dev/null ||
			{
				[ -z "$DEBUG" ] || echo couldn\'t decrypt "$f" possibly just a binary file
				rm $outpath
			}
	fi
done
echo decrypted files stored at "$TMP_PATH"
