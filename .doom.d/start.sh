#!/bin/bash

echo starting emacs...

. ~/.profile
[ -e ~/.venv ] && source ~/.venv/bin/activate
cd ~/.emacs.d
# this is a quirk with projectile failing to load if a project folder
# doesn't exist
mkdir -p /tmp/__site

# export EMACS=$(which emacs)
run_emacs() {
	# $EMACS --fg-daemon=server
	if [ -v TOOLBOX_PATH ]; then
		if [ -v SYSTEMD_EXEC_PID ]; then
			/usr/bin/toolbox run /usr/bin/emacs $@
		else
			/usr/bin/emacs $@
		fi
	else
		/usr/bin/toolbox run /usr/bin/emacs $@
	fi
}

run_emacs -ib 0 -bw 0 $@
