set nocompatible
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'jQuery'
" Syntax highlight
Bundle 'cucumber.zip'
Bundle 'Markdown'

" Git integration
Bundle 'git.zip'
Bundle 'fugitive.vim'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'kien/ctrlp.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'VimClojure'
Bundle 'Twinside/vim-syntax-haskell-cabal'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'SuperTab'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'vim-scripts/FuzzyFinder'

" non github repos
Bundle 'git://git.wincent.com/command-t.git'

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

colorscheme smyck
if has("gui_running")
    set guioptions=egmrt
endif


match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

let g:EasyMotion_leader_key = '<Leader>'
map <Leader>s :FufBuffer<CR>

:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar

