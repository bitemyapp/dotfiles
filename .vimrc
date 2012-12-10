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
" set tabstop=4
" set shiftwidth=4
set softtabstop=4
" set expandtab
" set autoindent
" set smartindent
" set smarttab
" set number

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
" set hlsearch                    " highlight matches
set incsearch                   " incremental searching
set ignorecase                  " searches are case insensitive...
set smartcase                   " ... unless they contain at least one capital letter

" Disable backup, swap file
set nobackup
set nowritebackup
set noswapfile

let os = substitute(system('uname'), "\n", "", "")

noremap <leader>ss :call StripWhitespace()<CR>

nnoremap <silent> <C-l> :nohl<CR><C-l>

noremap <leader>W :w !sudo tee % > /dev/null<CR>

noremap <leader>b :CtrlPBuffer<CR>

if has("autocmd")
	" Enable file type detection
	filetype on
	" Treat .json files as .js
	autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
endif

set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc

if has("gui_running")
    set guioptions=egmrt
    set guioptions-=T
endif

if os == "Darwin"
	set guifont=SourceCodePro:h12
else
	set guifont=Monospace:h9
endif

colorscheme molokai

