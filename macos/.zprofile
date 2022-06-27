eval "$(/opt/homebrew/bin/brew shellenv)"

# Adds `~/.local/bin` and all subdirectories to $PATH
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"
