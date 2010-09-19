#!/bin/zsh

if [[ -z $DISPLAY ]] && [[ $(tty) == "/dev/tty1" ]]; then
    clear
    startx -- -nolisten tcp -deferglyphs 16 &> /dev/null
    logout
fi

