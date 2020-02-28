# Turn on color for ls
alias ls='ls --color=always --group-directories-first'

# Generate m3u Playlists
alias m3ump3='ls -1v |grep .mp3 > '
alias m3um4a='ls -1v |grep .m4a > '
alias m3uflac='ls -1v |grep .flac > '

# Set vim mode
set -o vi

# directory shortcuts
alias g4='cd /mnt/4TBext4'
alias gn='cd ~/Documents/notes'
alias gf='cd ~/Documents/notes/finances'
alias gd='cd ~/Documents'
alias gD='cd ~/Dropbox'
alias gm='cd ~/Music'

# alias for working with config files in git bare repo
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias cfgp='/usr/bin/git --git-dir=$HOME/.cfgp/ --work-tree=$HOME'

# misc aliases
alias w3md='w3m duckduckgo.com'
alias td='vim ~/Documents/notes/org.md'
alias tmacs='emacs -nw'
alias tvim='vim -c "colorscheme default"'

# Set history to infinite
HISTSIZE=
HISTFILESIZE=
