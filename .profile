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
export MC_XDG_OPEN=~/.scripts/nohup-open

echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

# Start graphical server on TTY1 if i3 not already running.
[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x i3 >/dev/null && exec startx
