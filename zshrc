#!/bin/zsh

# Dircolors
eval `dircolors -b`

# Exports
export EDITOR=vim
export VISUAL=vim
export BROWSER=firefox
export OOO_FORCE_DESKTOP=gnome
export GDK_USE_XFT=1
export QT_XFT=true
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
export PATH="$(which ruby && ruby -rubygems -e "puts Gem.user_dir")/bin:${HOME}/.bin:${HOME}/.cabal/bin:${PATH}"
export PYTHONDOCS=/usr/share/doc/python/html/

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

setopt hist_reduce_blanks
setopt hist_ignore_space
setopt append_history
setopt extended_history
setopt hist_ignore_all_dups
setopt prompt_subst

typeset -ga precmd_functions
typeset -ga preexec_functions

autoload -U compinit && compinit
autoload -U colors && colors
autoload -Uz vcs_info

zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select=2
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:kill:*:jobs' verbose yes
zstyle ':vcs_info:*' branchformat "%b"
zstyle ':vcs_info:*' actionformats "(%s/%b/%a)"
zstyle ':vcs_info:*' formats "(%s/%b)"
zstyle ':vcs_info:git:*' actionformats " %(%b/%a)"
zstyle ':vcs_info:git:*' formats "(%b)"

PROMPT='%(?..%{$fg[red]%}%? )%(#.%{$fg_bold[red]%}.%{$fg[blue]%})%n%{$fg_no_bold[default]%}@%{$fg[green]%}%m%{$fg[default]%}:%{$fg[cyan]%}%35<...<%~%(1v. %{$fg[yellow]%}%1v .)%{$fg[default]%}%(#.#.%%) '

# Really short shortcuts
alias s='sudo'
alias p='sudo pacman'
alias a='sudo aura'
alias v='vim'
alias l='lesser'
alias x='extract'
alias -g L='| less'
alias -g Q='&> /dev/null &'

# Aliases
alias sudo='sudo -E'
alias ..='cd ..'
alias ls='ls -bhp --file-type --color=auto'
alias ll='ls -l'
alias la='ls -lA'
alias mv='mv -v --backup=existing'
alias rm='rm -iv'
alias cp='cp -v'
alias grep='grep --color=auto'
alias psg='ps -ef | grep -v grep | grep '
alias lsg='la | grep '
alias mkdir='mkdir -p'
alias vless='/usr/share/vim/vim73/macros/less.sh'
alias proxy='sshuttle -r b52@reaktor42.de --dns 0/0'
alias paste='fb'
alias hibernate='systemctl hibernate'
alias suspend='systemctl suspend'
alias matlab='matlab -nosplash'

# Functions
function extract() {
    [[ $# != 1 ]] && echo "Usage: $0 FILE\nExtract FILE to the current directory." && return 1
    [[ ! -f $1 ]] && echo "$1: Is not a valid file" && return 1
    
    case $1 in
        *.tar.gz|*.tgz)
            tar xvzf $1 ;;
        *.tar.bz2|*.tbz2)
            tar xvjf $1 ;;
        *.tar|*.gz|*.bz2|*.zip|*.rar|*.7z)
            7z x $1 ;;
        *)
            echo "$1: No tool available to extract"
            exit 1
    esac
}

function mplayer() {
    xautolock -disable
    xset -dpms
    =mplayer $@
    xset +dpms
    xautolock -enable
}

function lesser() {
    if [[ $# == 0 ]]; then
        ll .
        return $?
    fi

    [[ $# != 1 ]] && echo "Usage: $0 PATH\nHandle PATH in a proper way" && return 1
    [[ ! -e $1 ]] && echo "$1: Directory or file not found" && return 1

    cmd=()

    if [[ ! -r $1 ]]; then
        cmd[1]="s"
    fi

    if [[ -f $1 ]]; then
        case $1 in
            *.log|*.err|*.warn|*.info)
                cmd[2]="less" ;;
            *)
                cmd[2]="vless" ;;
        esac
    elif [[ -d $1 ]]; then
        cmd[2]="ll"
    else
        echo "$1: No idea how to handle that"
        return 1
    fi

    cmd[3]="$1"
    eval ${(j: :)cmd}
}

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

precmd_functions+='title'
precmd_functions+='vcs_pre'
preexec_functions+='title'

