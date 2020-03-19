#!/bin/sh

[ -f "$HOME/.xprofile" ] && . ~/.xprofile

exec tdm --xstart
