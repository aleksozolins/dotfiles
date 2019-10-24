# Turn on color for ls
alias ls='ls --color=always --group-directories-first'

# Generate m3u Playlists
alias m3ump3='ls -1v |grep .mp3 > '
alias m3um4a='ls -1v |grep .m4a > '
alias m3uflac='ls -1v |grep .flac > '

# Set vim mode
set -o vi

# alias for working with config files in git bare repo
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias cfgp='/usr/bin/git --git-dir=$HOME/.cfgp/ --work-tree=$HOME'

# misc aliases
alias w3md='w3m duckduckgo.com'

# Set history to infinite
HISTSIZE=
HISTFILESIZE=
