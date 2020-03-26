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

# tell Midnight Commander to use the nohup script to detach when executing files
export MC_XDG_OPEN=~/.local/bin/nohup-open

echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

# Switch escape and caps if tty:
sudo -n loadkeys ~/.config/ttymaps.kmap 2>/dev/null

# Start TDM on TTY1 to select an X session
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
