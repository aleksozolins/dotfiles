#!/bin/sh
clear

echo "This will install basic programs to reprovision your system based on basics.txt"
echo "Already installed programs will not be reinstalled"
echo "NOTE: BEFORE YOU REBOOT OR START X, MAKE SURE TO INSTALL A TERMINAL EMULATOR"

read -p "Press Enter to begin..."

sudo pacman -S --needed - < basics.txt
