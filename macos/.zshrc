# Source aliasrc if it exists
if [ -f ~/.config/aliasrc ]; then
    source ~/.config/aliasrc
fi

# Stuff for Zapier CLI
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# direnv hook (Needed by monorepo)
# eval "$(direnv hook zsh)"

