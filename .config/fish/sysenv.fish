#
# This file is parsed by pam_env module
#
# Syntax: simple "KEY=VAL" pairs on separate lines
#

## system
# set SCREEN DVI-D-2
if set -q WSLENV
    set XDG_RUNTIME_DIR /mnt/wslg/runtime-dir
else if ! set -q FGC
    set XDG_RUNTIME_DIR /run/user/1000
end
# set __GL_THREADED_OPTIMIZATIONS 1
# set R600_DEBUG sbcl,hyperz,llvm,sisched,forcedma
# set -e R600_DEBUG __GL_THREADED_OPTIMIZATIONS

## theming
set -q GTK@_RC_FILES || set GTK2_RC_FILES "$HOME/.gtkrc-2.0"
set -q QT_QPA_PLATFORMTHEME || set QT_QPA_PLATFORMTHEME qt5ct

# clipboard
function clc
	if set -q WSLENV
            cd /
            wex clip.exe
            cd -
	else 
    	wl-copy
        end
end
