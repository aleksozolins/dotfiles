#!/bin/bash
# Profile file. Runs on login.

# Adds `~/.local/bin` and all subdirectories to $PATH
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# More variables to export
export EDITOR="vim"
export TERMINAL="st"
export BROWSER="firefox"
export READER="mupdf"
export FILE="ranger"

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config/"
export XDG_CACHE_HOME="$HOME/.cache/"
export XDG_DATA_HOME="$HOME/.local/share/"

# homedir cleanup
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export LESSHISTFILE="-"
export GTK2_RC_FILES="$XDG_DATA_HOME/gtk-2.0/gtkrc-2.0"
export NOTMUCH_CONFIG="$XDG_DATA_HOME/notmuch-config"

# tell Midnight Commander to use the nohup script to detach when executing files
export MC_XDG_OPEN=~/.local/bin/nohup-open

echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

# switch escape and caps if tty:
sudo -n loadkeys ~/.config/ttymaps.kmap 2>/dev/null

# start TDM on TTY1 to select an X session
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && pullask && exec tdm

# An if statement is used to get the proper sequence. It's messy but it works.
if [[ "$(tty)" = "/dev/tty2" ]] && ! pgrep -x tmux >/dev/null; then
  dropbox &
  sleep 5
  clear
  pullask
  exec tmux
fi

# Start console TDM (this is the recommended way)
# source /usr/bin/tdm
