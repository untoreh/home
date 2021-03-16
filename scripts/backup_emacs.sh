#!/bin/sh
rsync=/usr/bin/rsync
$rsync -r ~/.doom.d/ ~/.tmp/doom.d/
ssh mbx mkdir -p /volatile/backup/dotfiles/doom.d/
$rsync -r ~/.doom.d/ mbx:/volatile/backup/dotfiles/doom.d/
ssh mbx mkdir -p /backups/dotfiles/doom.d/
$rsync -r ~/.doom.d/ mbx:/backups/dotfiles/doom.d/
