source ~/.vim/vimrc-python.org
let python_highlight_all=1

if &term =~ '^\(xterm\|screen\)$' && $COLORTERM == 'gnome-terminal'
	set t_Co=256
endif
let g:inkpot_black_background = 1
colorscheme inkpot

filetype on
filetype plugin on
filetype indent on
syntax on

set tabstop=4
set softtabstop=4
set shiftwidth=4
set et
set cindent
set smarttab
set ml

autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala setfiletype vala
au BufRead,BufNewFile *.vapi setfiletype vala
let vala_comment_strings=1
let vala_space_errors=1

" for latexsuite
set grepprg=grep\ -nH\ $*
