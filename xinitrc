#!/bin/sh

# Key management
gpg_info="${HOME}/.gpg_info"
if test -f ${gpg_info} && kill -0 $(cut -d: -f 2 ${gpg_info} | head -n 1) 2>/dev/null; then
    eval $(cat ${gpg_info})
else
    eval `/usr/bin/gpg-agent --daemon --enable-ssh-support --write-env-file ${gpg_info}`
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
