#!/bin/bash

source ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#alias ls='ls --color=auto'
source ~/.bash_prompt
PS1='[\u@\h \W]\$ '


# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

[ -f ~/.fzf.bash ] && . ~/.fzf.bash


# export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
# export GDK_SCALE=1
