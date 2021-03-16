#!/usr/bin/sudo /bin/zsh

DOM=${1:-win10}

kb=${prefix}/kvm/topre.xml
mouse=${prefix}/kvm/roccat.xml

## wait for domain to be off
if [ "$1" != force ]; then
    while ! timeout 3 virsh dominfo "$1" | grep 'running'; do
        echo waiting for domain to start
        sleep 1
    done
fi

virsh attach-device $DOM "$kb" --live
virsh attach-device $DOM "$mouse" --live
