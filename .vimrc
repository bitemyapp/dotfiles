set nocompatible               " be iMproved
filetype off                   " required!

" set rtp+=~/.vim/bundle/vundle/
" call vundle#rc()

" let Vundle manage Vundle
" Bundle 'gmarik/vundle'

" Bundle 'tpope/vim-fugitive'
" Bundle 'Lokaltog/vim-easymotion'
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
" Bundle 'L9'
" Bundle 'FuzzyFinder'
" Bundle 'git://git.wincent.com/command-t.git'

" noremap <leader>b :FufBuffer<CR>
set hidden
syntax on
filetype plugin indent on
set clipboard=unnamed " Use the OS clipboard
set number " Enable line numbers
set noerrorbells " Disable error bells
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter

" Disable backup, swap file
set nobackup
set nowritebackup
set noswapfile

if has("gui_running")
    set guioptions=egmrt
    set guioptions-=T
endif

set background=light
colorscheme vilight

