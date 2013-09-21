# b52's dotfiles

This repository is a collection of all necessary configuration files and
related data to run my [archlinux], [xmonad], mostly [shell]-based setup.

![snapshot of b52's setup](http://reaktor42.de/~b52/shots/2013-09-21-223903_1280x800_scrot.png)

The README shall also be a reminder of how to properly set it up, involving
the needed packages and configuration steps.

[archlinux]: https://www.archlinux.org/
[xmonad]: http://xmonad.org/
[shell]: http://www.zsh.org/

## Installation

1. Install some or all packages listed in the second chapter.
2. Checkout this repository ``git clone https://github.com/b52/dotfiles.git``
3. Link your choice of configuration files: ``cd dotfiles ; ./install.py *``
   Run ``./install.py --help`` to get a list of possible commands.

## Packages

The following packages are required in order to run the setup as it is
intended.

* zsh, tmux, gnupg2, rxvt-unicode-patched, gvim
* xmonad, xmonad-contrib, stalonetray, xmobar
* compton-git, notify-osd, xautolock, slock
* ttf-dejavu, proggyfonts, ttf-ms-fonts, ttf-liberation
* elementary-icon-theme, gnome-icon-theme
* adwaita-x-dark-and-light-theme, xcursor-vanilla-dmz-aa
* cower, fb-client
* gmrun, parcellite,

The following packages are just a reminder for myself.

* chrony, acpid
* ttf-droid, ttf-microsoft-arial
* dropbox, dropbox-cli
* texlive-most

## Additional Instructions

### Qt4 theme

Use _qtconfig-qt4_ to tell Qt4 to use the GTK+ theme and font.

### SSH key management

gpg-agent can handle GPG keys as well as SSH keys. In order to use your SSH key
with gpg-agent you have to run ``ssh-add`` once to store the key's fingerprint
in ~/.gnupg/sshcontrol.

