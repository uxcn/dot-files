" theme
colorscheme solarized
let g:solarized_termcolors=256
set background=dark

" vundle
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'edkolev/tmuxline.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
Plugin 'rking/ag.vim'

filetype plugin indent on

" vim
set nocompatible

set smartindent
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set textwidth=80
set pastetoggle=<F2>

set number
syntax on
filetype indent on

" airline
let g:airline#extensions#tabline#enabled = 1

" clang-format
map <c-k> :pyf ~/.vim/python/clang-format.py<cr>
imap <c-k> <esc>:pyf ~/.vim/python/clang-format.py<cr>i
nmap <F5> :%pyf ~/.vim/python/clang-format.py<cr>

" clang-check
nmap <silent> <F10> :call ClangCheck()<cr><cr>

" ultisnips
let g:UltiSnipsExpandTrigger="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" fugitive
autocmd QuickFixCmdPost *grep* cwindow
