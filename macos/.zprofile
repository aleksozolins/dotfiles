# eval "$(/opt/homebrew/bin/brew shellenv)

# Adds `~/.local/bin` and all subdirectories to $PATH
for dir in "$HOME/.local/bin" $(find "$HOME/.local/bin" -type d); do export PATH="$dir:$PATH"; done

# Setting PATH for Python 2.7
# When I reinstalled Python 2.7 manually, it added this
# PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
# export PATH
