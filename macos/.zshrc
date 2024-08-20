# Source aliasrc if it exists
if [ -f ~/.config/aliasrc ]; then
    source ~/.config/aliasrc
fi

# Stuff for Zapier CLI
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# npm Token for Zapier
source ~/.npm_token

# make sure npm token is set before call to initialize pyenv:
eval "$(pyenv init -)"

# Initialize pyenv here
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

# disabled this to with with the Zapier CLI
# . /opt/homebrew/opt/asdf/libexec/asdf.sh

# direnv hook (Needed by monorepo)
eval "$(direnv hook zsh)"
