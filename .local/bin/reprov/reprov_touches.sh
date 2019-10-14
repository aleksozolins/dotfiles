#!/bin/bash
echo "This will put finishing touches on your installation in the following ways:"
echo "1: Import your public and private GPG key"
echo "2: Configure pam-gnupg-git"
echo "3: Enable a cron job to check your mail periodically"
echo "4: Authenticate goobook"
echo "NOTE: Please make sure you have run both reprov_basics.sh and reprov_aur_basics.sh and are presently running in X"
echo "NOTE: Please make sure you have signed into Dropbox and sync'd your GPG directory"

read -p "Press Enter to begin..."

# ask about syncing mail accounts.
echo "Would you like to synchronize all your mail accounts? yes or no ?"
read mailsync

# import your GPG keys
gpg --import ~/Dropbox/GPG/aleks_ozolins_public_gpg_key.txt
gpg --import ~/Dropbox/GPG/aleks_ozolins_private_gpg_key.asc

# initialize the password store
pass init aleksozolins

# make changes to /etc/pam.d/system-local-login as root
echo "auth      optional  pam_gnupg.so" | sudo tee -a /etc/pam.d/system-local-login
echo "session   optional  pam_gnupg.so" | sudo tee -a /etc/pam.d/system-local-login

# enable cron job for mutt wizard
mw cron

# mbsync all accounts if yes
if [[ $mailsync == y* ]]
  then
  mbsync aleksozolins
  mbsync icloud
  mbsync thingsforsale
  else
  echo "moving on..."
fi

# authenticate goobook
goobook authenticate

echo "If you didn't see any errors, you should be all set!!!"
echo "Be sure to check ~/reprov_todo.txt for final configuration tasks."
echo "Some things you might want to do now:" >> ~/reprov_todo.txt
echo "-Configure intel-ucode for microcode" >> ~/reprov_todo.txt 
echo "-Configure powertop.service" >> ~/reprov_todo.txt
echo "-Configure Thunderbird email" >> ~/reprov_todo.txt
echo "-Configure your GTK themes with LXappearance" >> ~/reprov_todo.txt
echo "-Login to Firefox" >> ~/reprov_todo.txt
echo "-Set your screenlayouts using arandr. default.sh and docked.sh. Remember to set wallpapers there too." >> ~/reprov_todo.txt
