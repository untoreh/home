#!/bin/sh
#
# The "pre-rebase" hook is run just before "git rebase" starts doing
# its job, and can prevent the command from running by exiting with
# non-zero status.
#
# The hook is called with the following parameters:
#
# $1 -- the upstream the series was forked from.
# $2 -- the branch being rebased (or empty when rebasing the current branch).
#

branch="$2"
[ -n "$branch" ] || branch=`git rev-parse --abbrev-ref HEAD`

lock="branch.${branch}.rebaselock"

if [ x`git config --bool "$lock"` = xtrue ]; then
    echo "pre-rebase hook: \"$lock\" is set to true. Refusing to rebase."
    exit 1
fi
