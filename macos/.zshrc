# Source aliasrc if it exists
if [ -f ~/.config/aliasrc ]; then
    source ~/.config/aliasrc
fi

# Stuff for Zapier CLI
# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Make sure my shell uses fnm for node
# eval "$(fnm env)"

# npm Token for Zapier
source ~/.npm_token

# make sure npm token is set before call to initialize pyenv:
# eval "$(pyenv init -)"

# Start tat script (tmux) if it isn't running
#_not_inside_tmux() { [[ -z "$TMUX" ]] }

#ensure_tmux_is_running() {
#  if _not_inside_tmux; then
#    tat
#  fi
#}

# ensure_tmux_is_running
