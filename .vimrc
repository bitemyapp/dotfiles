set nocompatible
filetype off                   " required!

filetype plugin indent on     " required!

" Make backspace behave in a sane manner.
set backspace=indent,eol,start
set tabstop=4
set autoindent
set copyindent
set shiftwidth=4
set expandtab
set shiftround
set showmatch
set ignorecase
set smartcase
set incsearch

set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell
set noerrorbells

set nobackup
set noswapfile

" call pathogen#infect()
" Switch syntax highlighting on
syntax on

inoremap jj <Esc>

colorscheme slate
if has("gui_running")
    set guioptions=egmrt
endif


match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar

