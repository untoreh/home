#!/bin/sh

BID=$1
[ -n "$BID" ] || { echo build id not provided; exit 1; }
FV=${FGC/f}
[ -n "$FV" ] || { FV=$2; }
[ -n "$FV" ] || { echo fedora version not provided; exit 1; }
ARCH=$(uname -m)
VERSION="29.1-0.2"
PKG="emacs-filesystem"

URL="https://download.copr.fedorainfracloud.org/results/bhavin192/emacs-pretest/fedora-$FV-$ARCH/$BID-emacs/$PKG-$VERSION.fc$FV.noarch.rpm"

rpm-ostree override replace "$URL"
