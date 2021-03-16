#!/usr/bin/sudo /bin/zsh

## kill kodi bacause it leaves listening ports hanging
# kill -9 $(pgrep kodi); kill -9 $(pgrep quasar)
## stop X if using only 1 gpu
# systemctl stop sddm
# systemctl stop sway@7
# logout from kde
user=fra
su fra qdbus org.kde.ksmserver /KSMServer logout 0 0 0

DOM=win10
prefix=/home/fra
domainxml=/etc/libvirt/qemu/win10.xml
logs=${prefix}/.tmp/log
genxml=${prefix}/.bin/virshxmlgen
kb=${prefix}/kvm/topre.xml
mouse=${prefix}/kvm/roccat.xml
vgatovfio=${prefix}/kvm/vgatovfio.sh

modprobe -a vfio vfio-pci
sleep 1
zsh ${vgatovfio} &>>$logs

## cpu tuning governor
modprobe msr ## cpufreq monitor driver
modprobe -a virtio virtio-pci virtio-net virtio-scsi virtio-blk virtio-balloon virtio-ring ## virtio
echo "schedutil" | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

## ram tuning (16GB)
echo 3 | sudo tee /proc/sys/vm/drop_caches
echo 1 | sudo tee /proc/sys/vm/compact_memory
echo 8192 | sudo tee /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages

echo "# Attach plugged usb device to running vm
ACTION==\"add\", SUBSYSTEM==\"usb\", ENV{DEVTYPE}==\"usb_device\", RUN+=\"$genxml %s{idVendor} %s{idProduct} $DOM\"" > /etc/udev/rules.d/00-vm.rules

## delete video devices from domain
cat <<< "$(${HOME}/.local/bin/xq -x 'del(.domain.devices.graphics,.domain.devices.video)' $domainxml)" > $domainxml
systemctl restart libvirtd

udevadm control --reload-rules


## start vm
virsh start win10

wait
