#!/bin/bash -l

MAC=00:15:5d:36:4f:49
DEVICE=eth0

sudo ip link set $DEVICE down
sudo macchanger -m $MAC $DEVICE
sudo ip link set $DEVICE up
supc restart dhclient
