#!/bin/sh

# Key management
gpg_agent_info = "${HOME}/.gnupg/gpg-agent.info"
if pgrep -u "${USER}" gpg-agent >/dev/null 2>&1; then
    eval `cat ${gpg_agent_info}`
    eval `cut -d= -f1 ${gpg_agent_info} | xargs echo export`
else
    eval `gpg-agent --daemon`
fi

# Some visuals i wanna have really fast
xsetroot -cursor_name left_ptr &
hsetroot -fill ~/.wallpapers/current &

# Blend Qt into GTK environment
export GTK2_RC_FILES="${HOME}/.gtkrc-2.0"

# Start xmonad
exec ck-launch-session xmonad
