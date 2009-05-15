source ~/.vim/vimrc-python.org
let python_highlight_all=1

set t_Co=256
set background=dark
colorscheme desert256

filetype on
filetype plugin on
filetype indent on
syntax on

set tabstop=4
set softtabstop=4
set shiftwidth=4
set noet
set cindent
set smarttab

autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala setfiletype vala
au BufRead,BufNewFile *.vapi setfiletype vala
let vala_comment_strings=1
let vala_space_errors=1
