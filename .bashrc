#!/bin/bash

source ~/.profile

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#alias ls='ls --color=auto'
source ~/.bash_prompt
PS1='[\u@\h \W]\$ '


[ -f ~/.fzf.bash ] && . ~/.fzf.bash
fzf_file=/usr/share/fzf/shell/key-bindings.bash
[ -f $fzf_file ] && source $fzf_file
unset fzf_file
