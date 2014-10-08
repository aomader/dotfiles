#!/bin/zsh

if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] ; then
    clear
    exec startx -- -nolisten tcp -deferglyphs 16 vt1
fi

