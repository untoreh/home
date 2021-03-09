#!/bin/bash
# github.com/shalva97/kde-configuration-files

set -e

name=$(date +%D | sed 's/\//-/g')
backup_path=~/plasma-backups/${name}

cd ~/.config
mkdir -p $backup_path

cp -a kde* ${backup_path}/

k_files=$(find . -name 'k*' -maxdepth 1 -type f)
cp -a ${k_files} ${backup_path}/

cp -a plasma* ${backup_path}/

cp -a gtkrc* ${backup_path}/

cp -a Trolltech.conf ${backup_path}/

cp -a baloo* ${backup_path}/

cp -a device_automounter_kcmrc ${backup_path}/

cp -a powermanagementprofilesrc ${backup_path}/

cp -a bluedevilglobalrc ${backup_path}/
