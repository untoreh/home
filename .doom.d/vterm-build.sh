#!/usr/bin/env bash

set -e
VERSION=$(emacs --version | head -1 | awk '{print $3}')
rm -rf ~/.emacs.d/.local/straight/build-$VERSION/vterm/build
mkdir ~/.emacs.d/.local/straight/build-$VERSION/vterm/build
cd ~/.emacs.d/.local/straight/build-$VERSION/vterm/build
cmake -DUSE_SYSTEM_LIBVTERM=no ..

