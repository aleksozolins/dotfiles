#!/bin/bash
echo "This will put finishing touches on your installation in the following ways:"
echo "1: Import your public and private GPG key"
echo "2: Configure pam-gnupg-git"
echo "3: Enable a cron job to check your mail periodically"
echo "4: Authenticate goobook"
echo "NOTE: Please make sure you have run both reprov_basics.sh and reprov_aur_basics.sh and are presently running in X"
echo "NOTE: Please make sure you have signed into Dropbox and sync'd your GPG directory"

read -p "Press Enter to begin..."

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

# authenticate goobook
goobook authenticate

echo "If you didn't see any errors, you should be all set!"
echo "Some things you might want to do now:"
echo "1. Configure intel-ucode for microcode"
echo "2. Configure powertop.service"
echo "3. Configure Thunderbird email"
echo "4. Configure your GTK themes with LXappearance"
echo "5. Login to Firefox"
echo "6. mbsync all your mail accounts before running neomutt. IE mbsync aleksozolins."
