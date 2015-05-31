set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
" set rtp+=~/.vim/bundle/Vundle.vim
" call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
" Plugin 'gmarik/Vundle.vim'

" Plugin 'tpope/vim-fugitive'
" Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
" Plugin 'kien/ctrlp.vim'
" " Plugin 'scrooloose/nerdtree'
" Plugin 'dag/vim2hs'
" Plugin 'tpope/vim-commentary'
" Plugin 'tpope/vim-sensible'
" Plugin 'scrooloose/syntastic'
" Plugin 'bling/vim-airline'
" Plugin 'mhinz/vim-signify'
" Plugin 'plasticboy/vim-markdown'
" Plugin 'vim-scripts/buftabs'
" Plugin 'godlygeek/tabular'
" Plugin 'eagletmt/ghcmod-vim'
" Plugin 'Shougo/vimproc'
" Plugin 'Lokaltog/vim-easymotion'
" Plugin 'tpope/vim-fireplace'
" Plugin 'guns/vim-clojure-static'
" Plugin 'ingydotnet/yaml-vim'

" Themes
" Plugin 'tomasr/molokai'
" Plugin 'altercation/vim-colors-solarized'
" Plugin 'flazz/vim-colorschemes'

" All of your Plugins must be added before the following line
" call vundle#end()            " required
filetype plugin indent on    " required

set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows
set wildignore+=*.hi,*.o,*.dyn*
set wildignore+=*/dist/*

" To ignore plugin indent changes, instead use:
"filetype plugin on

" Brief help
" :PluginList          - list configured plugins
" :PluginInstall(!)    - install (update) plugins
" :PluginSearch(!) foo - search (or refresh cache first) for foo
" :PluginClean(!)      - confirm (or auto-approve) removal of unused plugins
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
set swapfile
set dir=/tmp

" let mapleader = ","
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

"set guifont=Monaco:h14
colorscheme slate

