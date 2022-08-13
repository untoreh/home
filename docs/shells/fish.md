## "Ghost hosts" in fish shell autocompletion
Fish shell hosts autocompletion for the `ssh` command doesn't just pull hosts from `~/.ssh/config` but also from
`~/.ssh/known_hosts`. If you have some stale hosts in the latter file, it will keep suggesting them until you delete them.
