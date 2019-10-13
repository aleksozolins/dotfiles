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
cd ~/Dropbox/GPG
gpg --import aleks_ozolins_public_gpg_key.txt
gpg --import aleks_ozolins_private_gpg_key.asc

# make changes to /etc/pam.d/system-local-login as root
sudo -i
echo "auth      optional  pam_gnupg.so" >> /etc/pam.d/system-local-login
echo "session   optional  pam_gnupg.so" >> /etc/pam.d/system-local-login
exit

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
