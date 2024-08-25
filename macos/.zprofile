# Added for Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# For programs that look for config files there like tmux
export XDG_CONFIG_HOME="$HOME/.config"

# Adds `~/.local/bin` and all subdirectories to $PATH
for dir in "$HOME/.local/bin" $(find "$HOME/.local/bin" -type d); do export PATH="$dir:$PATH"; done
