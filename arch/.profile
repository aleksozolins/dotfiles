#!/bin/bash
# Profile file. Runs on login.

# Adds `~/.local/bin` and all subdirectories to $PATH -- Hmm... This doesn't work any more
# export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/bin/statusbar:$PATH"

# More variables to export
export ALTERNATE_EDITOR="nvim"
export EDITOR="emacsclient -t -a"
export VISUAL="emacsclient -c -a emacs"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export FILE="pcmanfm"

# XDG base directories
export XDG_CONFIG_HOME="$HOME"/.config/
export XDG_CACHE_HOME="$HOME"/.cache/
export XDG_DATA_HOME="$HOME"/.local/share/

# export XDG user directories using the xdg-user-dir config file
eval "$(sed 's/^[^#].*/export &/g;t;d' ${DIR:-$XDG_CONFIG_HOME}/user-dirs.dirs)"

# homedir cleanup
export XINITRC="$XDG_CONFIG_DIR"/X11/xinitrc
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/password-store
export LESSHISTFILE="-"
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc-2.0
# export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuch-config
[ -f $HOME/.lesshst ] && rm $HOME/.lesshst
# [ -f $HOME/.Xauthority ] && rm $HOME/.Xauthority
export TASKRC="$XDG_CONFIG_HOME"/task/taskrc
export TASKDATA="$HOME"/Dropbox/apps/task

# bash config cleanup
export HISTFILE="$XDG_DATA_HOME/history"
[ -f $HOME/.bash_logout ] && rm $HOME/.bash_logout
[ -f $HOME/.bash_history ] && rm $HOME/.bash_history

# midnight commander
export MC_XDG_OPEN="$HOME"/.local/bin/nohup-open

# tmux
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
 
# source bashrc
echo "$0" | grep "bash$" >/dev/null && [ -f $HOME/.config/bashrc ] && source "$HOME/.config/bashrc"

# set caps lock to control if tty:
sudo -n loadkeys $XDG_CONFIG_HOME/ttymaps.kmap 2>/dev/null

# Ensure the system clock is accurate
sudo timedatectl set-ntp true

# start DWM if TTY1
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x dwm >/dev/null && exec startx "$XDG_CONFIG_HOME/X11/xinitrc"
