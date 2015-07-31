set nocompatible              " be iMproved, required
filetype off                  " required

filetype plugin indent on    " required

set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
set wildignore+=*.hi,*.o,*.dyn*
set wildignore+=*/dist/*

" To ignore plugin indent changes, instead use:
"filetype plugin on

set swapfile
set dir=/tmp

let mapleader = ","
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>l :GhcModCheckAndLintAsync<CR>

set noeb vb t_vb=
au GUIEnter * set vb t_vb=
set mouse=c
set nofoldenable
let g:haskell_conceal = 0
" Tab specific option
set tabstop=2                   "A tab is 8 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=2               "Insert 4 spaces when tab is pressed
set shiftwidth=2                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs

set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar

set incsearch
set nohlsearch

" packages
set runtimepath^=~/.vim/bundle/ctrlp.vim

execute pathogen#infect()

syntax on
filetype plugin indent on

colorscheme desert
