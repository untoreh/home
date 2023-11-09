#!/bin/bash

echo starting emacs...

. ~/.profile
[ -e ~/.venv ] && source ~/.venv/bin/activate
cd ~/.emacs.d
# this is a quirk with projectile failing to load if a project folder
# doesn't exist
mkdir -p /tmp/__site

systemdvar(){
	systemctl --user import-environment WAYLAND_DISPLAY,XAUTHORITY,XDG_CURRENT,XDG_SEAT,XDG_SESSION_CLASS,XDG_SESSION_TYPE,XDG_VTNR
}

# export EMACS=$(which emacs)
run_emacs() {
	# $EMACS --fg-daemon=server
	if [ -v TOOLBOX_PATH ]; then
		if [ -v SYSTEMD_EXEC_PID ]; then
			systemdvars
			exec /usr/bin/toolbox run /usr/bin/emacs $@
		else
			exec /usr/bin/emacs $@
		fi
	else
		exec /usr/bin/toolbox run /usr/bin/emacs $@
	fi
}

run_emacs -mm -ib 0 -bw 0 $@
