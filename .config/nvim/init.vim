" theme
colorscheme zenburn
set background=dark

" vundle
filetype off
set rtp+=/home/jason/.config/nvim/bundle/vundle
call vundle#rc()

Plugin 'bling/vim-airline'
Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-unimpaired'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'rking/ag.vim'
Plugin 'uxcn/vim-x2x'
Plugin 'simnalamburt/vim-mundo'
Plugin 'christoomey/vim-tmux-navigator'

filetype plugin indent on

" nvim
set nocompatible
set encoding=utf-8

set wildmenu
set autoread

set scrolloff=1
set sidescrolloff=5

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

set smarttab
set smartindent
set showcmd


set shiftround
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set textwidth=80


set incsearch

set number
set relativenumber
syntax on

" airline
let g:airline_theme='lucius'
let g:airline#extensions#tabline#enabled=1

" ultisnips
let g:UltiSnipsExpandTrigger="<c-y>"
let g:UltiSnipsJumpForwardTrigger="<c-i>"
let g:UltiSnipsJumpBackwardTrigger="<c-o>"

"search
nnoremap <silent> <leader>n :%s///gn<cr>
nnoremap <silent> <c-n>     :nohlsearch<cr>

" paste
inoremap <silent> <f3> <c-o>:set nopaste<cr><c-r>+
inoremap <silent> <f4> <c-o>:set paste<cr><c-r>+<c-o>:set nopaste<cr>

" eclim
let g:EclimCompletionMethod = 'omnifunc'

" youcompleteme
nnoremap     <silent> <leader>c  :YcmCompleter GoToDeclaration<cr>
nnoremap     <silent> <leader>cc :YcmCompleter GoToDefinition<cr>

" clang-format
noremap  <silent> <leader>f           :pyf ~/.config/nvim/python/clang-format.py<cr>
inoremap <silent> <leader>f      <c-o>:pyf ~/.config/nvim/python/clang-format.py<cr>
nnoremap <silent> <leader>ff          :%pyf ~/.config/nvim/python/clang-format.py<cr>

" gundo
nnoremap <silent> <leader>g :GundoToggle<cr>

" fugitive
nnoremap <silent> <leader>dgt   :diffget //2<cr>
nnoremap <silent> <leader>dgm   :diffget //3<cr>

autocmd QuickFixCmdPost *grep* cwindow

" tasks
nnoremap <silent> <leader>t :Ag \(FIXME\)\\|\(TODO\)<cr>

" x2x
xnoremap <leader>b  <plug>x2b
nnoremap <leader>b  <plug>x2b
xnoremap <leader>o  <plug>x2o
nnoremap <leader>o  <plug>x2o
xnoremap <leader>d  <plug>x2d
nnoremap <leader>d  <plug>x2d
xnoremap <leader>h  <plug>x2h
nnoremap <leader>h  <plug>x2h
