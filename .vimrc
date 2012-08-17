set nocompatible
set esckeys
set backspace=indent,eol,start " Allow backspace in insert mode

set wildmenu " command-line completion
set clipboard=unnamed " Use the OS clipboard

set hidden " handle multiple buffers more efficiently
set ttyfast
set gdefault " Add the g flag to search/replace by default

set encoding=utf-8 nobomb

" Make tabs four spaces
set expandtab
set tabstop=4

" Show “invisible” characters
" set lcs=tab:▸\ ,trail:·,eol:¬,nbsp:_
" set list

" Don’t add empty newlines at the end of files
set binary
set noeol

" Respect modeline in files
set modeline
set modelines=4

set number " Enable line numbers

syntax on " Enable syntax highlighting

set noerrorbells " Disable error bells

set nostartofline " Don’t reset cursor to start of line

set ruler " Show the cursor position

set shortmess=atI " No intro

set showmode
set title
set showcmd
set scrolloff=3

"" Searching
set hlsearch                    " highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter

" Centralize backups, swapfiles and undo history
set backupdir=~/.vim/backups
set directory=~/.vim/swaps
set undodir=~/.vim/undo

"" Searching
set hlsearch                    " highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter


"" Plugins
call pathogen#infect()          " load pathogen
call pathogen#helptags()        " load pathogen helptags
" let g:loaded_netrwPlugin=1      " disable netrw plugin
" let g:Powerline_symbols='fancy' " disable netrw plugin

function! StripWhitespace()
	let save_cursor = getpos(".")
	let old_query = getreg('/')
	:%s/\s\+$//e
	call setpos('.', save_cursor)
	call setreg('/', old_query)
endfunction
noremap <leader>ss :call StripWhitespace()<CR>

noremap <leader>W :w !sudo tee % > /dev/null<CR>

nnoremap ,f :Ack<space>

if has("autocmd")
	" Enable file type detection
	filetype on
	" Treat .json files as .js
	autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
endif

if has("gui_running")
    set guioptions=egmrt
endif

"" GUI
set guioptions-=T               " Toggle off toolbar
set guifont=Mensch\ for\ Powerline:h14
set transparency=5

colorscheme molokai
" color vilight
