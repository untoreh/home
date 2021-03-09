#!/usr/bin/fish

systemctl --user show-environment | grep -i SSH_ | while read ssh_env
    set var (string split "=" $ssh_env)
    set -x $var[1] $var[2]
end


