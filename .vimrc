" why exactly is this needed?
" we are not compatible with old vi
set nocompatible
set laststatus=2

" Use pathogen to easily modify the runtime path to include all
" plugins under the ~/.vim/bundle directory
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" change the mapleader from \ to ,
let mapleader=","

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" hide buffers on :e instead of forcing a write
set hidden
" fix backspace to work as usually (delete character to the left of the cursor)
set backspace=indent,eol,start
set nowrap        " don't wrap lines
set tabstop=4     " a tab is four spaces
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on
		  " autoindenting
set number        " always show line numbers
set shiftwidth=4  " number of spaces to use for
		  " autoindenting
set shiftround    " use multiple of shiftwidth when
		  " indenting with '<' and '>'
set showmatch     " set show matching parenthesis
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all
		  " lowercase,
		  "    case-sensitive otherwise
set smarttab      " insert tabs on the
		  " start of a line according to
                  "   shiftwidth, not tabstop
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type
set history=1000  "remember more commands and search history
set undolevels=1000 " use many levels of undo
set wildignore=*.swp,*.bak,*.class
set title        " change terminal title
set visualbell   " do not beep
set noerrorbells " do not beep
set nobackup

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" use !!w for writing root owned file when editing as normal user
cmap w!! w !sudo tee % >/dev/null

syntax on
filetype plugin indent on " enable plugins on file extensions
set mouse=a "enable mouse?
"use ; as well as : for commands
nnoremap ; :  	

set termencoding=utf-8
set encoding=utf-8

" use bash like completion with tab 
set wildmenu
set wildmode=list:full

" VimClojure settings
let vimclojure#ParenRainbow = 1
let vimclojure#WantNailgun = 0
let vimclojure#NailgunClient = '/home/dienst/clojure/vimclojure-nailgun-client/ng.exe'
" enable 256 colors
set t_Co=256
colorscheme lucius
