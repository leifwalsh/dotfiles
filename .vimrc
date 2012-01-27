set nocompatible

source ~/.vim/vimrc-python.org
let python_highlight_all=1

set tabstop=4
set backspace=indent,eol,start

set autoindent
set cindent
set copyindent
set shiftwidth=4
set softtabstop=4
set showmatch
set ignorecase
set smartcase
set smarttab
set hlsearch
set incsearch
set expandtab
set ml

set history=5000
set undolevels=5000
set wildignore=*.swp,*.bak,*.pyc,*.class,*.o,*.so,*.la,*.a
set title
set visualbell
set noerrorbells

filetype on
filetype plugin indent on

set background=dark
colorscheme solarized
if &t_Co > 2 || has("gui_running")
    syntax on
endif

" load the types.vim highlighting file, if it exists
autocmd BufRead,BufNewFile *.[ch] let fname = expand('<afile>:p:h') . '/types.vim'
autocmd BufRead,BufNewFile *.[ch] if filereadable(fname)
autocmd BufRead,BufNewFile *.[ch]   exe 'so ' . fname
autocmd BufRead,BufNewFile *.[ch] endif

" noweb
au BufRead,BufNewFile *.nw set filetype=noweb
let noweb_backend = "tex"
let noweb_language = "c"
let noweb_fold_code = 0

" for latexsuite
set grepprg=grep\ -nH\ $*

" restore last position when opening a buffer
set viminfo='1000,:1000,@1000,/1000
au BufWinLeave /* mkview
au VimEnter /* silent loadview
