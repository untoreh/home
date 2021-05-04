#
# This file is parsed by pam_env module
#
# Syntax: simple "KEY=VAL" pairs on separate lines
#

## system
set FLOOR "ubu"
set SCREEN DVI-D-2
if set -q WSLENV
	set XDG_RUNTIME_DIR /mnt/wslg/runtime-dir
else
	set XDG_RUNTIME_DIR /run/user/1000
end
set -q GDK_BACKEND || set GDK_BACKEND wayland
# set __GL_THREADED_OPTIMIZATIONS 1
# set R600_DEBUG sbcl,hyperz,llvm,sisched,forcedma
# set -e R600_DEBUG __GL_THREADED_OPTIMIZATIONS

# set LD_LIBRARY_PATH $LD_LIBRARY_PATH:/usr/lib64:/usr/lib:/usr/lib:$HOME/.local/share/Steam/ubuntu12_32/steam-runtime/amd64/lib/x86_64-linux-gnu:$HOME/.local/share/Steam/ubuntu12_32/steam-runtime/i386/lib/i386-linux-gnu

## theming
set -q GTK@_RC_FILES || set GTK2_RC_FILES "$HOME/.gtkrc-2.0"
set q QT_QPA_PLATFORMTHEME || set QT_QPA_PLATFORMTHEME "qt5ct"

## firefox
set SLIMERJSLAUNCHER /usr/bin/firefox
set MOZ_USE_OMTC 1 ## for firefox off main thread compositing

# clipboard
if [ -n "$DISPLAY" ]
	if [ -n "$WSLENV" ]
		function clc
			clip.exe
		end
	else
    	function clc
        	xclip -i -selection clipboard
    	end
    	end
else
    alias clc wl-copy
end

