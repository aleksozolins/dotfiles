#!/bin/bash
# Profile file. Runs on login.

# Adds `~/.local/bin` and all subdirectories to $PATH
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# More variables to export
export EDITOR="vim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export READER="mupdf"
export FILE="ranger"

# Start the Calibre server (only installed on NZXT!)
# [ -f /usr/bin/calibre ] && calibre-server --enable-auth --port=8888 &

# tell Midnight Commander to use the nohup script to detach when executing files
export MC_XDG_OPEN=~/.local/bin/nohup-open

echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

# Switch escape and caps if tty:
sudo -n loadkeys ~/.config/ttymaps.kmap 2>/dev/null

# Start graphical server on TTY1 if i3 not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x i3 >/dev/null && pullask && exec startx

# Start tmux and associated programs on TTY2 if it is not already running.
# [ "$(tty)" = "/dev/tty2" ] && ! pgrep -x tmux >/dev/null && pullask && exec tmux
# instead, an if statement is used to get the proper sequence. It's messy but it works.

if [[ "$(tty)" = "/dev/tty2" ]] && ! pgrep -x tmux > /dev/null; then
  dropbox &
  sleep 5
  clear
  pullask
  exec tmux
fi
