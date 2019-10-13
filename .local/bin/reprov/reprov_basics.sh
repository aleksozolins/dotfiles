#!/bin/sh
echo "This will install basic programs to reprovision your system based on basics.txt"
echo "Make sure you run this script as user, and that you have sudo privileges"
echo "Already installed programs will not be reinstalled"

read -p "Press Enter to begin..."

sudo pacman -S --needed - < basics.txt

# enable network manager
echo "Would you like to enable NetworkManager? yes or no?"
read nm
if [[ $nm == y* ]]
  then
  sudo systemctl enable --now NetworkManager
  else
  echo "That's fine, live your life without internet. What do I care?"
fi

# does ~/repos exist?
if [ ! -d ~/repos ]
  then
  mkdir ~/repos
  else
  echo "~/repos already exists!"
fi

# install a terminal emulator
echo "Would you like to install Luke Smith's build of ST? NOTE: You may have to adjust the config.h file afterwords and reinstall. yes or no?"
read te
if [[ $te == y* ]]
  then
  cd ~/repos && git clone https://github.com/LukeSmithxyz/st.git && cd ~/repos/st && sudo make install 
  else
  echo "OK, but don't blame me when you can't get a prompt..."
fi

# enable cronie for cron jobs
echo "Would you like to enable the cronie service for cron jobs? yes or no?"
read cronie
if [[ $cronie == y* ]]
  then
  sudo systemctl enable cronie 
  else
  echo "Whatever that's fine..."
fi

echo "Think about running reprov_aur_basics.sh now if you want more shit to work..."
