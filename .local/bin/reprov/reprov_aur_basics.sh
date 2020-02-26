#!/bin/sh
echo "This will install yay and basic AUR programs to reprovision your system"
echo "Make sure you have sudo privileges before running this script. Best to have already run reprov_basics.sh"
echo "NOTE: DO NOT USE THIS SCRIPT IF YAY IS ALREADY INSTALLED"

read -p "Press Enter to begin..."

# ask about creating the maildirs
echo "Would you like to create the top level mail directories in ~/.local/share/mail/? yes or no ?"
read maildirs

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
makepkg -si --noconfirm

echo "yay installed!"

# install programs
yay -S --noconfirm ttf-joypixels ttf-symbola dropbox mutt-wizard-git pam-gnupg-git goobook-git breeze-default-cursor-theme geekbench nestopia gtk-theme-arc-gruvbox-git

# recreate the top level mail directories if yes
if [[ $maildirs == y* ]]
  then
  mkdir ~/.local/share/mail && mkdir ~/.local/share/mail/aleksozolins && mkdir ~/.local/share/mail/icloud && mkdir ~/.local/share/mail/thingsforsale
  else
  echo "That's fine we'll just move on then..."
fi

# next steps?
echo "Why don't you reboot, sync your Dropbox GPG folder, and run reprov_touches.sh ok?"
