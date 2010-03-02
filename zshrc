#!/bin/zsh

# Dircolors
eval `dircolors -b`

# Exports
export EDITOR=vim
export VISUAL=vim
export OOO_FORCE_DESKTOP=gnome

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

# Colorful manpages using less
export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

# Key bindings
bindkey "\e[1~" beginning-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[3~" delete-char
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history

# Completion
autoload -U compinit && compinit
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always
zstyle ':completion:*' squeeze-slashes true

# Define the prompt
setopt prompt_subst
autoload -U colors && colors
autoload -Uz vcs_info

zstyle ':vcs_info:*' branchformat "%b"
zstyle ':vcs_info:*' actionformats "(%s/%b/%a)"
zstyle ':vcs_info:*' formats "(%s/%b)"
zstyle ':vcs_info:git:*' actionformats " %(%b/%a)"
zstyle ':vcs_info:git:*' formats "(%b)"

function title() {
    local access
    local cmd

    if [[ $USERNAME != $LOGNAME || -n $SSH_CLIENT ]]; then
        access="${USERNAME}@${HOST}: "
    fi

    if [[ $# -eq 0 ]]; then
        cmd="zsh %30<...<%~"
    else
        cmd="${(V)1//\%/\%\%}"
    fi

    case $TERM in
        rxvt*)
            print -Pn "\e]0;${access}${cmd}\e\\"
            ;;
        screen*)
            print -Pn "\ek${cmd}\e\\"
            print -Pn "\e_${access}\e\\"
            ;;
    esac
}

function vcs_pre() {
    PSVAR=""
    vcs_info
    [[ -n $vcs_info_msg_0_ ]] && PSVAR[1]="$vcs_info_msg_0_"
}

typeset -ga precmd_functions
typeset -ga preexec_functions

precmd_functions+='title'
precmd_functions+='vcs_pre'
preexec_functions+='title'

PROMPT='%(?..%{$fg[red]%}%? )%(#.%{$fg_bold[red]%}.%{$fg[blue]%})%n%{$fg_no_bold[default]%}@%{$fg[green]%}%m%{$fg[default]%}:%{$fg[cyan]%}%35<...<%~%(1v. %{$fg[yellow]%}%1v .)%{$fg[default]%}%(#.#.%%) '

# Aliases
alias sudo='sudo -E'
alias s='sudo'
alias p='sudo pacman-color'

alias ..='cd ..'
alias ls='ls -bhp --file-type --color=auto'
alias ll='ls -l'
alias la='ls -lA'
alias mv='mv -v --backup=existing'
alias rm='rm -iv'
alias cp='cp -v'
alias grep='grep --color=auto'
alias psg='ps -ef | grep '
alias llg='ll | grep '

alias pwr='dbus-send --system --print-reply --dest="org.freedesktop.Hal" /org/freedesktop/Hal/devices/computer org.freedesktop.Hal.Device.SystemPowerManagement.Shutdown'
alias rbt='dbus-send --system --print-reply --dest="org.freedesktop.Hal" /org/freedesktop/Hal/devices/computer org.freedesktop.Hal.Device.SystemPowerManagement.Reboot'

# Functions
function power() {
    [[ $# -ne 2 ]] && echo 'hehejo'
}
