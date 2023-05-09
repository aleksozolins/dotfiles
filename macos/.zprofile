eval "$(/opt/homebrew/bin/brew shellenv)"

# Adds `~/.local/bin` and all subdirectories to $PATH
for dir in "$HOME/.local/bin" $(find "$HOME/.local/bin" -type d); do export PATH="$dir:$PATH"; done
