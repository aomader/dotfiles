" load our vim/bundle stuff
call pathogen#infect()

" lets use the internet standard charset
set encoding=UTF-8

" syntax highlighting
syntax on
colorscheme awesomeness

" also yank to X clipboard, requires +X11 +clipboard
set clipboard=unnamedplus

" indention rules
set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set backspace=indent,eol,start

" visuals
set list
set listchars=tab:→\ ,extends:►,precedes:◄
set formatoptions+=tc
set ruler
set rulerformat="%l/%L (%p%%), %c"
set showmatch
set display+=lastline
set number
set cursorline

" search
set hls
set ignorecase
set smartcase

" folding
set foldmethod=indent
set foldlevel=99

" window focus movement
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h

" python pep8 and validation
let g:pyflakes_use_quickfix=0

" paste on C-p using lodgeit
map <C-p> :Lodgeit<CR>

" NERDTree settings
let NERDTreeChDirMode=2
let NERDTreeIgnore=['\.vim$', '\~$', '\.pyc$', '^\.']
let NERDTreeShowBookmarks=1
map <F3> :NERDTreeToggle<CR>


" python related settings
au FileType py set textwidth=79
au FileType py set smartindent
" c related settings
au FileType c set textwidth=79

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
