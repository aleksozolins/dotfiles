#!/bin/sh
clear

echo "This will install yay and basic AUR programs to reprovision your system"

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

#install programs
yay -S ttf-joypixels ttf-symbola dropbox mutt-wizard-git pam-gnupg-git goobook-git

#warn about configs
echo "REMEMBER TO IMPORT GPG KEY, CONFIGURE PAM-GNUPG-GIT, AUTHENTICATE GOOBOOK, RECREATE THE TOP LEVEL MAILDIRS (~/local/share/mail/aleksozolins, icloud, thingsforsale) AND SET UP CRON JOBS BEFORE RUNNING NEOMUTT"
