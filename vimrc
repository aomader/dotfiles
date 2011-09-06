" lets use the internet standard charset
set encoding=UTF-8

" syntax highlighting
syntax on
colorscheme awesomeness
set hls
set cursorline

" show linenumbers
set number

" indention rules
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set ruler
set rulerformat="%l/%L (%p%%), %c"
set showmatch
set display+=lastline

" visualize non printable chars
set nowrap
set list
set listchars=tab:→\ ,extends:►,precedes:◄

" folding
set foldmethod=indent
set foldlevel=99

" python pep8 and validation
let g:pyflakes_use_quickfix=0

" tabline
function MyTabs()
    let s = ''
    for i in range(tabpagenr('$'))
        let n = i + 1
        if n == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        let buflist = tabpagebuflist(n)
        let winnr = tabpagewinnr(n)

        let s .= '%' . (i + 1) . 'T'
        let s .= ' ' . n . '.' . bufname(buflist[winnr - 1]) .' '
    endfor

    let s .= '%#TabLineFill#%T'

    return s
endfunction
set tabline=%!MyTabs()

filetype on
filetype plugin indent on
call pathogen#infect()

" intellisense, what else?
" autocmd FileType c set omnifunc=ccomplete#Complete
" autocmd FileType css set omnifunc=csscomplete#CompleteCSS
" autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
" autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
" autocmd FileType php set omnifunc=phpcomplete#CompletePHP
" autocmd FileType python set omnifunc=pythoncomplete#Complete
" autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags

