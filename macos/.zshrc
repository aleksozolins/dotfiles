# add color and functionality to core-utils
# alias ls='ls -hN --color=always --group-directories-first'
# alias grep='grep --color=auto'
# alias diff='diff --color=auto'
# alias sudo='sudo '

# Generate m3u Playlists
# alias m3ump3='ls -1v |grep .mp3 > '
# alias m3um4a='ls -1v |grep .m4a > '
# alias m3uflac='ls -1v |grep .flac > '

# Set vim mode
set -o vi

# directory shortcuts
alias gf='cd ~/Dropbox/docs/finances'
alias gd='cd ~/Dropbos/docs'
alias gD='cd ~/Dropbox'
alias gm='cd ~/Dropbox/mus'
alias gp='cd ~/Dropbos/pics'
alias gv='cd ~/Dropbos/vids'
alias gr='cd ~/repos'

# homedir cleanup
alias dosbox='dosbox -conf "$XDG_CONFIG_HOME"/dosbox/dosbox.conf'
alias tmux='tmux -f ~/.config/tmux/tmux.conf'
# alias startx='startx "$XDG_CONFIG_HOME"/X11/xinitrc'

# misc aliases
alias cfg="git -C ~/.dotfiles"
alias e="nvim"
alias tvim='nvim -c "colorscheme default"'
alias trans="tremc"
alias hdc="ls -a ~ | wc -l"
alias logout='pkill -u'
alias vim='nvim'
alias w3md='w3m duckduckgo.com'
alias cls='clear'
alias newsboat='newsboat -c $HOME/Dropbox/apps/newsboat/cache.db'
alias yt='pipe-viewer'
alias ytdl='youtube-dl'
# alias solitaire='ttysolitaire --no-background-color'
# alias newapps='sudo pacman -S --needed - < ~/repos/reprov_new/pacman_reprov.txt'
# alias newtix='cp $HOME/docs/icanotes/tix/tix.txt $HOME/docs/icanotes/tix/"20$(date +%y)-$(date +%m)-$(date +%d).txt" && vim $HOME/docs/icanotes/tix/"20$(date +%y)-$(date +%m)-$(date +%d).txt"'
# alias yay='echo USE PARU'
# alias db='dropbox-cli status'
alias stow='stow --no-folding'

# Set history to infinite
# HISTSIZE=
# HISTFILESIZE=

_not_inside_tmux() { [[ -z "$TMUX" ]] }

ensure_tmux_is_running() {
  if _not_inside_tmux; then
    tat
  fi
}

ensure_tmux_is_running
