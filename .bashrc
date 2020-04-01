# Turn on color for ls and group directories first
alias ls='ls --color=always --group-directories-first'

# Generate m3u Playlists
alias m3ump3='ls -1v |grep .mp3 > '
alias m3um4a='ls -1v |grep .m4a > '
alias m3uflac='ls -1v |grep .flac > '

# Set vim mode
set -o vi

# directory shortcuts
alias g4='cd /mnt/4TBext4'
alias gn='cd ~/docs/notes'
alias gf='cd ~/docs/notes/finances'
alias gd='cd ~/docs'
alias gD='cd ~/Dropbox'
alias gm='cd ~/mus'
alias gp='cd ~/pics'
alias gv='cd ~/vids'
alias gr='cd ~/repos'

# alias for working with config files in git bare repo
alias cfg='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias cfgp='/usr/bin/git --git-dir=$HOME/.cfgp/ --work-tree=$HOME'

# homedir cleanup
alias dosbox='dosbox -conf "$XDG_CONFIG_HOME"/dosbox/dosbox.conf'
alias tmux='tmux -f "$XDG_CONFIG_HOME"/tmux/tmux.conf'
alias abook='abook --config "$XDG_CONFIG_HOME"/abook/abookrc --datafile "$HOME"/Dropbox/apps/abook/addressbook'

# misc aliases
alias w3md='w3m duckduckgo.com'
alias td='vim ~/Documents/notes/org.md'
alias tvim='vim -c "colorscheme default"'
alias trans="transmission-remote-cli"
alias hdc="ls -a ~ | wc -l"
alias logout='pkill -u'
alias vim='nvim'

# Set history to infinite
HISTSIZE=
HISTFILESIZE=
