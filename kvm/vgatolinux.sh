#!/bin/zsh

[ -z "$1" ] && { echo please specify a guest to wait shutdown; exit 1; }
DEVICES=(
    "0000:20:00.0"
    "0000:20:00.1"
)

## wait for domain to be off
if [ "$1" != force ]; then
    while timeout 3 virsh dominfo "$1" | grep 'running'; do
        echo waiting for domain to shutdown or suspend
        sleep 1
    done
fi

## unbind vfio and rescan
echo removing listed pci devices
for d in $DEVICES; do
    echo 1 > "/sys/bus/pci/devices/$d/remove"
done

sleep 1
echo rescanning pci devices
echo 1 > /sys/bus/pci/rescan
