#!/bin/sh
## wait for samba service to be online

host=${1:-localhost}
port=${2:-139}
while :;do
    nc -z $host $port && sleep 2 && break;
    sleep 1;
done

