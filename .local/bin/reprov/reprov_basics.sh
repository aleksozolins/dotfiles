#!/bin/sh
echo "This will install basic programs to reprovision your system based on basics.txt"
echo "Make sure you run this script as user, and that you have sudo privileges"
echo "Already installed programs will not be reinstalled"

read -p "Press Enter to begin..."

# ask about st
echo "Would you like to install Luke Smith's build of st? NOTE: You may have to adjust the config.h file afterwords and reinstall. yes or no?"
read te

# ask about cron jobs
echo "Would you like to enable the cronie service for cron jobs? yes or no?"
read cronie

# ask about touchpad/trackpoint
echo "Is this a Thinkpad with a trackpoint? Do you need xf86-input-synaptics? yes or no?"
read synaptics

# ask about throttling fix for x1
echo "Is this your Thinkpad Carbon X1? Do you need to install and enable the throttling fix?? yes or no?"
read throttled

# ask about broadcom-wl
echo "Do you need broadcom wireless, maybe for the X61? yes or no?"
read broadcom

# ask about Nvidia
echo "Do you need that evil Nvidia driver? yes or no?"
read nvidia

sudo pacman -S --needed - < basics.txt

# does ~/repos exist?
if [ ! -d ~/repos ]
  then
  mkdir ~/repos
  else
  echo "~/repos already exists!"
fi

# install a terminal emulator if yes
if [[ $te == y* ]]
  then
  cd ~/repos && git clone https://github.com/LukeSmithxyz/st.git && cd ~/repos/st && sudo make install 
  else
  echo "OK, but don't blame me when you can't get a prompt..."
fi

# enable cronie for cron jobs if yes
if [[ $cronie == y* ]]
  then
  sudo systemctl enable cronie 
  else
  echo "Whatever that's fine..."
fi

# install synaptics if yes
if [[ $synaptics == y* ]]
  then
  sudo pacman -S xf86-input-synaptics
  else
  echo "moving on..."
fi

# install throttling fix if yes
if [[ $throttled == y* ]]
  then
  sudo pacman -S throttled
  sudo systemctl enable --now lenovo_fix.service
  else
  echo "moving on..."
fi

# install broadcom-wl if yes
if [[ $broadcom == y* ]]
  then
  sudo pacman -S broadcom-wl
  else
  echo "moving on..."
fi

# install nvidia if yes
if [[ $nvidia == y* ]]
  then
  sudo pacman -S nvidia
  else
  echo "moving on..."
fi

echo "Think about running reprov_aur_basics.sh now if you want to install more..."

cd
