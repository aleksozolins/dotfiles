"     _    ___
"    / \  / _ \   Aleks Ozolins
"   / _ \| | | |  aleks@aleksozolins.com
"  / ___ \ |_| |  https://www.aleksozolins.com
" /_/   \_\___/
"
" .vimrc
"
let mapleader =","

" use vim mode instead of pure vi; it must be the first instruction
set nocompatible

" required by Vundle
filetype off
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin('~/.config/nvim/bundle')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" install plugins here
Plugin 'vim-airline/vim-airline'
Plugin 'morhetz/gruvbox'
Plugin 'potatoesmaster/i3-vim-syntax'
Plugin 'plasticboy/vim-markdown'
Plugin 'junegunn/goyo.vim'
Plugin 'tpope/vim-commentary'
" Plugin 'jreybert/vimagit' " Check this out later
Plugin 'scrooloose/nerdtree'
Plugin 'kovetskiy/sxhkd-vim'
Plugin 'ap/vim-css-color'
Plugin 'christoomey/vim-tmux-navigator'

" remove the trailing whitespace warning from airline
let g:airline#extensions#whitespace#enabled = 0

" all plugins must be added before the following line
call vundle#end()
filetype plugin indent on  " again required by Vundle

" Nerd tree
map <leader>n :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1

" filetype settings
" filetype on " Vundle needs this to be off apparently
filetype plugin on
filetype indent on

" display settings
set encoding=utf-8
set showmatch
set number relativenumber

" search settings
set hlsearch
set ignorecase
set smartcase

" set vim-specific sequences for RGB colors; for some reason, colors aren't
" quite right without these two lines. They must be set before the colorscheme
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" syntax highliting
set termguicolors
colorscheme gruvbox
set bg=dark
syntax enable

" mouse and clipboard settings
set mouse=a
set clipboard=unnamedplus

" enable autocompletion:
set wildmode=longest,list,full

" spell checking
" set spell

" global tab width (added for python course)
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab

" Set filetype tab settings (added for python course)
autocmd FileType python,doctest set ai ts=4 sw=4 sts=4 et

autocmd VimResized * :wincmd =
" Zoom a vim pane, <C-w>= to re-balance
nnoremap <leader>- :wincmd _<cr>:wincmd \|<cr>
nnoremap <leader>= :wincmd =<cr>

" Run commands in a tmux split
Plugin 'christoomey/vim-tmux-runner'
let g:VtrDetachedName = "detached"
let g:VtrPercentage = 15

nmap <silent> <C-e>o :VtrOpenRunner<cr>
nmap <silent> <C-e>f :VtrFocusRunner<cr>
nmap <silent> <C-e>a :VtrAttachToPane<cr>
nmap <silent> <C-e>d :VtrSendCtrlD<cr>
nmap <silent> <C-e>c :VtrClearRunner<cr>
vmap <silent> <C-e>c :VtrClearRunner<cr>
nmap <silent> <C-e>x :VtrKillRunner<cr>
nmap <silent> <C-e>e :VtrSendLinesToRunner<cr>
" need this one in visual mode for visually highlighted code blocks
vmap <silent> <C-e>e :VtrSendLinesToRunner<cr>
