syntax on
colorscheme slate
set ruler
set nocompatible
set modelines=0

set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set ttyfast
set backspace=indent,eol,start

let mapleader ="\\"

set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

set wrap
set textwidth=79


nnoremap j gj
nnoremap k gk

inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

nnoremap ; :

au FocusLost * :wa
" use spaces, not tabs
set sw=4 sts=4 ts=4 et
let g:CommandTMaxFiles=20000
set wildignore+=*.o,*.obj,.git,*.png,*.jpg,*.swp,*.bak,*.pyc,*.class,*.gif

if has("gui_running")
    set guioptions=egmrt
    set guifont=Inconsolata
endif


set noerrorbells         " don't beep
set nobackup
set noswapfile
set history=1000         " remember more commands and search history
set undolevels=1000      " use many muchos levels of undo
filetype on
filetype plugin on
filetype indent on

autocmd filetype python set expandtab
au FileType py set autoindent
au FileType py set smartindent
au FileType py set textwidth=79 " PEP-8 Friendly
