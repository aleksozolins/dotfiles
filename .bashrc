# Turn on color for ls
alias ls='ls --color=always'

# Generate m3u Playlists
alias m3ump3='ls -1v |grep .mp3 > '
alias m3um4a='ls -1v |grep .m4a > '
alias m3uflac='ls -1v |grep .flac > '

# Set vim mode
set -o vi

# alias for working with config files in git bare repo
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# Set history to infinite
HISTSIZE=
HISTFILESIZE=
