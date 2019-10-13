#!/bin/sh
echo "This will install yay and basic AUR programs to reprovision your system"
echo "Make sure you have sudo privileges before running this script. Best to have already run reprov_basics.sh"
echo "NOTE: DO NOT USE THIS SCRIPT IF YAY IS ALREADY INSTALLED"

read -p "Press Enter to begin..."

# does ~/repos exist?
if [ ! -d ~/repos ]
  then
  mkdir ~/repos
  else
  echo "~/repos already exists!"
fi

# change to ~/repos
cd ~/repos

# clone into yay
git clone https://aur.archlinux.org/yay.git

# change to ~/repos/yay
cd ~/repos/yay

# install yay
makepkg -si

echo "yay installed!"

# install programs
yay -S ttf-joypixels ttf-symbola dropbox mutt-wizard-git pam-gnupg-git goobook-git breeze-default-cursor-theme light geekbench

# recreate the top level mail directories
echo "Would you like to create the top level mail directories in ~/.local/share/mail/? yes or no ?"
read maildirs
if [[ $maildirs == y* ]]
  then
  mkdir ~/.local/share/mail && mkdir ~/.local/share/mail/aleksozolins && mkdir ~/.local/share/mail/icloud && mkdir ~/.local/share/mail/thingsforsale
  else
  echo "That's fine we'll just move on then..."
fi

# add user to light group if running on laptop
echo "Are you on a laptop? If so, I'm going to add you to the light group so you have backlight control. yes or no ?"
read lightg
if [[ $lightg == y* ]]
  then
  sudo groupadd light
  sudo usermod -a -G light aleksozolins
  else
  echo "Alright, you must be running a desktop..."
fi

# next steps?
echo "Why don't you reboot, sync your Dropbox GPG folder, and run reprov_touches.sh ok?"
