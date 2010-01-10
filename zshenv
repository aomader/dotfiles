#!/bin/zsh

# Dircolors
eval `dircolors -b`

# Exports
export EDITOR=vim
export VISUAL=vim

# Colors
export NC='\e[0m'
export white='\e[0;30m'
export WHITE='\e[1;30m'
export red='\e[0;31m'
export RED='\e[1;31m'
export green='\e[0;32m'
export GREEN='\e[1;32m'
export yellow='\e[0;33m'
export YELLOW='\e[1;33m'
export blue='\e[0;34m'
export BLUE='\e[1;34m'
export magenta='\e[0;35m'
export MAGENTA='\e[1;35m'
export cyan='\e[0;36m'
export CYAN='\e[1;36m'
export black='\e[0;37m'
export BLACK='\e[1;37m'

# Key bindings
bindkey "\e[1~" beginning-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[3~" delete-char
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
