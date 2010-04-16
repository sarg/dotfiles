set nocompatible
set nobackup
set noswapfile 
set nowrap

set autoindent
set smartindent

set ruler
set incsearch
set hlsearch
set ttyfast
set showcmd
set showmatch

set expandtab
set smarttab
set tabstop=4
set shiftwidth=4
set shiftround
set softtabstop=4

set t_Co=256

set hidden

set wildmenu

set foldmethod=indent

set listchars=tab:>\ 
set list

set number
set showbreak=+\
set statusline=%<%f%h%m%r\ %b\ %{&encoding}\ 0x\ \ %l,%c%V\ %P 
set laststatus=2
set backspace=indent,eol,start
set textwidth=0
set viminfo='20,\"50
set history=50
set encoding=utf-8

"set langmap=йq,цw,уe,кr,еt,нy,гu,шi,щo,зp,х[,ъ],фa,ыs,вd,аf,пg,рh,оj,лk,дl,ж\\;,э',яz,чx,сc,мv,иb,тn,ьm,б\\,,ю.,ё`,ЙQ,ЦW,УE,КR,ЕT,НY,ГU,ШI,ЩO,ЗP,Х{,Ъ},ФA,ЫS,ВD,АF,ПG,РH,ОJ,ЛK,ДL,Ж:,Э\\",ЯZ,ЧX,СC,МV,ИB,ТN,ЬM,Б<,Ю>,Ё~

set scrolloff=5

set guifont=terminus
set guioptions=c

syntax on
filetype on
filetype plugin on
colorscheme Mustang
set fileencodings=utf-8,koi8-r

" tab switching
map <C-n> gt
map <C-p> gT 
map <C-t> :tabnew<CR>:FufFile<CR>
imap <C-t> <ESC>:tabnew<CR>:FufFile<CR>

map <C-o> :FufFile<CR>

nmap <Tab> v>
nmap <S-Tab> v<
xmap <Tab> >gv
xmap <S-Tab> <gv

cmap <C-a> <home>
cmap <C-e> <end>
cnoremap <C-b> <left>
cnoremap <C-d> <del>
cnoremap <C-f> <right>

map <F2> :w<CR>
map <F3> :wq<CR>
imap <F2> <Esc>:w<CR>i
imap <F3> <Esc>:wq<CR>

map <S-Insert> <MiddleMouse>

fun! s:perl_rc()
    setlocal equalprg=perltidy
    setlocal fdm=syntax

    set keywordprg=perldoc
    set makeprg=perl\ -c\ %\ $*
    let perl_include_pod = 1
    let perl_extended_vars = 1
    let perl_fold = 1
    let perl_fold_blocks = 1
endf

autocmd Filetype python :set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType perl :cal s:perl_rc()

let g:snippetsEmu_key = "<C-j>"

set complete=""
set complete+=.
set complete+=k
set complete+=b
set complete+=t

autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS

let g:fuf_autoPreview = 0
let g:fuf_file_exclude = '\v\~$|\.(pyc)$|(^|[/\\])\.(hg|git|bzr)($|[/\\])'
let g:fuf_previewHeight = 0
