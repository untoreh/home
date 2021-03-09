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


