#!/bin/sh

# Key management
if [ -f "${HOME}/.gpg-agent-info" ] && kill -0 $(cut -d: -f 2 "${HOME}/.gpg-agent-info" | head -n 1) 2>/dev/null; then
    eval $(cat ${HOME}/.gpg-agent-info)
else
    eval $(gpg-agent --daemon)
fi
export GPG_AGENT_INFO
export SSH_AUTH_SOCK
export SSH_AGENT_PID

# Some visuals i wanna have really fast
xsetroot -cursor_name left_ptr &
hsetroot -center ~/.wallpapers/current

# Start the proper WM
case $1 in
    xmonad)
        exec ck-launch-session xmonad
        ;;
    pekwm)
        exec ck-launch-session pekwm
        ;;
    *)
        exec ck-launch-session twm
        ;;
esac
