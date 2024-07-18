## Remote

# ## network
alias ssh "kitten ssh"
alias rtn "ssh -t root@192.168.1.1 SHELL=/bin/bash exec /bin/bash -li"
alias rft "ssh root@192.168.1.1 COLUMNS=\$COLUMNS LINES=\$LINES TERM=linux iftop -i br-lan -m 20m"
alias ift " _ iftop -i enp33s0 -m 20m"

alias mip "curl -s ipinfo.io/ip"
alias mipt "curl -s --proxy socks5://localhost:9050 ipinfo.io | jq"

# alias sshfs="mkdir -p \$rem_mount/$somename && sshfs -o cache=yes,auto_cache,kernel_cache,compression=yes,compressionlevel=6,large_read,big_writes,ciphers=aes128-gcm@openssh.com,allow_other -p 2468 core@$ip:/ \$rem_mount/\$somename"

alias maccon "remmina --connect $HOME/.remmina/1486221641829.remmina "
alias chgho "ssh localhost -c none"

function pgt
    set -q argv[1]; or set argv[1] "google.it"
    /bin/ping $argv[1] | grep --line-buffered 'time=' | sed -r 's/.*time=((.*)ms)/\1/'
end

## fetch the first hostname from ip (fragment)
function host_from_ssh
    set query $argv[1]
    sed -r "/\s*hostname.*($query)[0-9\.\s]*\$/Iq" <$HOME/.ssh/config | read -z first_host
    # echo $first_host
    echo $first_host | tac | read -z rev
    # echo $first_host
    echo $rev | sed -n -r '/^host /I{s/\s*host\s+//Ip;q;};' || echo "can only match ips :("
end

alias hossh host_from_ssh

## fetch hostname from hostname (fragment)
function complete_hostname_from_ssh
    set query $argv[1]
    set host1 (string replace -r '[0-9]' '' $query)
    set host2 (string replace -r '/[a-zA-Z]/g' '' $query)
    grep -Poi '\s*host\s*\K.*'"$host1"'.*'"$host2"'.*[^\s]*' ~/.ssh/config
end
alias chssh="complete_hostname_from_ssh"

## fetch ip from ssh config

function ip_from_ssh
    set -lq part
    [ "$argv[1]" = -p ] && set part Port && shift || set part HostName
    sed -n -e '/'"$argv[1]"'/I,/'$part'/Ip' <~/.ssh/config | read -z hostnames
    echo $hostnames | sed '/^#.*$/d' | read -z hostnames
    echo $hostnames | sed -n 's/\s*'$part'\s*//Ip' | read -z ips
    set ip (echo $ips | grep '.' | sort -u | tail -1)
    echo -n $ip
end
alias ipssh="ip_from_ssh"
alias possh="ip_from_ssh -p"

function ips_from_ssh
    set -q part
    [ $argv[1] = -p ] && set part Port && shift || set part HostName
    sed -n -e '/'"$1"'/I,/Host\s/{/^#.*$/d;s/.*'"$part"'\s*//Ip}' <~/.ssh/config | read -z ips
    echo $ips | sort -u
end
alias ipsSsh="ips_from_ssh"
