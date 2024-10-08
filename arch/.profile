#!/bin/bash
# Profile file. Runs on login.

# Adds `~/.local/bin` and all subdirectories to $PATH
for dir in "$HOME/.local/bin" $(find "$HOME/.local/bin" -type d); do export PATH="$dir:$PATH"; done

# More variables to export
export EDITOR="nvim"
export VISUAL="nvim"
export TERMINAL="alacritty"
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
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME"/notmuch-config
[ -f $HOME/.lesshst ] && rm $HOME/.lesshst

# bash config cleanup
export HISTFILE="$XDG_DATA_HOME/history"
[ -f $HOME/.bash_logout ] && rm $HOME/.bash_logout
[ -f $HOME/.bash_history ] && rm $HOME/.bash_history

# midnight commander
export MC_XDG_OPEN="$HOME"/.local/bin/nohup-open

# tmux
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
 
# source bashrc
[ -f "$HOME/.config/bashrc" ] && source "$HOME/.config/bashrc"

# set caps lock to control if tty:
sudo -n loadkeys $XDG_CONFIG_HOME/ttymaps.kmap 2>/dev/null

# Ensure the system clock is accurate
sudo timedatectl set-ntp true

# Start Emacs in daemon mode if not already running
if ! pgrep -u "$USER" -x "emacs" > /dev/null; then
    emacs --daemon &
fi

# start DWM if TTY1
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x dwm >/dev/null && exec startx "$XDG_CONFIG_HOME/X11/xinitrc"
