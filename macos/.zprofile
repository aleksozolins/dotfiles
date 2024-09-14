# Some environment variables
export EDITOR="nvim"
export VISUAL="nvim"
export XDG_CONFIG_HOME="$HOME/.config"

# Maybe disable this to with with the Zapier CLI
. /opt/homebrew/opt/asdf/libexec/asdf.sh

# npm Token for Zapier
source ~/.npm_token

# Initialize pyenv here
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

# Added for Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Adds `~/.local/bin` and all subdirectories to $PATH
for dir in "$HOME/.local/bin" $(find "$HOME/.local/bin" -type d); do export PATH="$dir:$PATH"; done
