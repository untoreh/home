#!/bin/sh
# This script automates tethering PHONE -> PC

# mnic=usb0
adb="adb -s 0000950458052835"
# mnic=enp3s0f0u8u3u2
hostnic=enp3s0f0u9u1
termnic=usb0
termuser=root
revtether=/data/sdext2/alpine/revtether.sh

## start ssh
timeout 5 $adb shell /data/sdext2/alpine/enter.sh
## wait for tethering
echo 'please enable tethering...'
$adb shell "am start -n com.android.settings/.TetherSettings"
while [ -z "$(ip -4 a show dev $hostnic | grep 'inet ')" ]; do sleep 1; done

## get ips
hostip="$(ip -4 a show dev $hostnic | awk '/inet/{print $2}')"
hostip24=${hostip%\.*}
hostipnomask=${hostip%\/*}
termip=${hostip24}.129
ssh="ssh ${termuser}@${termip}"

## routing
$ssh /sbin/ip a add dev usb0 ${termip}/24
$ssh /sbin/route add default gw ${hostipnomask} dev ${termnic}

sudo ip addr flush dev $hostnic
sudo ip addr add ${hostip}/24 dev $hostnic
echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward
sudo iptables -t nat -L POSTROUTING | grep -qE 'MASQUERADE\s+all\s+--\s+anywhere\s+anywhere' ||
	sudo iptables -t nat -A POSTROUTING -j MASQUERADE
