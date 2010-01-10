" lets use the internet standard charset
set encoding=UTF-8

" syntax highlighting
syntax on
colorscheme desert

" show linenumbers
set number

" indention rules
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" visualize non printable chars
set nowrap
set list
set listchars=tab:→\ ,extends:►,precedes:◄

" enable snipMate
filetype plugin on

" intellisense, what else?
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags

