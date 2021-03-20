#!/system/bin/sh

radioM=20
mobileDataM=27
wlanIface=wlan0

## turn on radio
service call phone $radioM i32 1
## turn on mobile data
service call phone $mobileDataM

## set mobile data as preferred network
service call connectivity $preferredNetworkM i32 0

## wait for mobile data to connect
while [ -z "$(ip a | grep ccmni | grep inet)" ]; do sleep 1; done

## restart wifi
ip link set $wlanIface up
## enable network bypassing android service
## service call phone 1 s16 *#*#526#*#*
## enable wifi for android watchdog
service call wifi 13 i32 1
echo enable_network 0 | wpa_cli -p /data/misc/wifi/sockets/ -i $wlanIface


