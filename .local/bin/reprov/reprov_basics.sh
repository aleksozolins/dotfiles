#!/bin/sh

clear

echo "This will install basic programs to reprovision your system based on basics.txt"
echo "Already installed programs will not be reinstalled"
read -p "Press Enter to begin..."
sudo pacman -S --needed - < basics.txt
