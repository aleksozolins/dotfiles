"     _    ___
"    / \  / _ \   Aleks Ozolins
"   / _ \| | | |  aleks@aleksozolins
"  / ___ \ |_| |  http://www.aleksozolins.com
" /_/   \_\___/
"
" .vimrc

" use vim mode instead of pure vi; it must be the first instruction
set nocompatible

" display settings
set encoding=utf-8
set showmatch
set number relativenumber

" search settings
set hlsearch
set ignorecase
set smartcase

" filetype settings
filetype on
filetype plugin on
filetype indent on

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
