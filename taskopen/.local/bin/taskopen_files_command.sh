#!/bin/sh

# Check the operating system
if [ "$(uname)" = "Darwin" ]; then
    # macOS uses 'open'
    open "$1"
else
    # Linux uses 'xdg-open' in the background with 'setsid'
    setsid xdg-open "$1" >/dev/null 2>&1 &
fi
