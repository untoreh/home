#!/bin/bash

source $HOME/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#alias ls='ls --color=auto'
source ~/.bash_prompt
PS1='[\u@\h \W]\$ '


# added by travis gem
[ -f /home/fra/.travis/travis.sh ] && source /home/fra/.travis/travis.sh

[ -f ~/.fzf.bash ] && . ~/.fzf.bash

[[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh # this line was added by RESH (Rich Enchanced Shell History)


# export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
# export GDK_SCALE=1
