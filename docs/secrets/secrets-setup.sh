#!/usr/bin/env bash
set -eo pipefail

GIT_BRANCH=${GIT_BRANCH:-master}
# encryption command
ecmd="git-ssl"

if ! type -p $ecmd >/dev/null; then
	echo encription command "($ecmd)" not found in path
	exit 1
fi

if [ -e .git ]; then
	echo -e this is already a git repository."\n" \
		To convert it to an encrypted repository "\n" \
		we need to start from scratch. "\n" \
		Delete the .git directory
	exit 1
fi

if [ -z "$PASSWORD" ]; then
	if [ -f ./pass ]; then
		PASSWORD=$(<./pass)
	else
		echo export a \$PASSWORD as recipient \
			or provide a \'pass\' file at point
		exit 1
	fi
fi
export PASSWORD CIPHER

git init
git config filter.$ecmd.smudge "$ecmd smudge"
git config filter.$ecmd.clean "$ecmd clean"
git config filter.$ecmd.required true
git config diff.$ecmd.textconv "$ecmd diff"
git config pull.ff only

if [ -z "$1" ]; then
	echo -e "\nNow you can configure the .gitattributes file, \n" \
		"(i.e. 'files/* filter=$ecmd diff=$ecmd') \n" \
		with the files in the repository that you want to encrypt.
	echo -e "\nfiles pushed to a remote repository \n" \
		matching git attributes will now be encrypted
	echo -e "\n do not push until .gitattributes is setup!"
else
	git remote add origin "$1"
	git fetch --depth=1 origin $GIT_BRANCH
	git checkout $GIT_BRANCH
fi

if [ -e rebaselock.pre-rebase.sh ]; then
	echo "Adding git pre-rebase locking hook.."
	ln -srf rebaselock.pre-rebase.sh ~/.secrets/.git/hooks/pre-rebase
	git config branch.master.rebaselock 1
else
	echo "Warning: pre-rebase locking hook not found!"
fi

echo "DONE."
