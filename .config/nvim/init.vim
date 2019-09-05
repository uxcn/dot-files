" vim-plug
call plug#begin('~/.config/nvim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Valloric/YouCompleteMe'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sleuth'
Plug 'rking/ag.vim'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'uxcn/vim-x2x'
Plug 'simnalamburt/vim-mundo'
Plug 'christoomey/vim-tmux-navigator'

Plug 'morhetz/gruvbox'

call plug#end()

" theme
colorscheme gruvbox
set background=dark

" transparency (background)
command! Transparent highlight Normal ctermbg=none guibg=none

filetype plugin indent on

" nvim
set nocompatible
set encoding=utf-8

set wildmenu
set autoread

set scrolloff=1
set sidescrolloff=5

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

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
set foldmethod=indent
set switchbuf=usetab,split
syntax on

" leader
let mapleader=","

" helpers
command! Filetypes :echo join(map(split(globpath(&rtp, 'ftplugin/*.vim'), '\n'), 'fnamemodify(v:val, ":t:r")'), "\n")

" init
command! Init :tabnew ~/.config/nvim/init.vim

" airline
let g:airline_theme='lucius'
let g:airline#extensions#tabline#enabled=1

" ultisnips
let g:UltiSnipsExpandTrigger="<c-y>"
let g:UltiSnipsJumpForwardTrigger="<c-i>"
let g:UltiSnipsJumpBackwardTrigger="<c-o>"

" commands
cabbrev vsbuffer vertical sbuffer
cabbrev help tab help

" navigation
nnoremap <silent> <leader>o  :open<space>
nnoremap <silent> <leader>t  :tabnew<cr>
nnoremap <silent> <leader>q  :quit<cr>
nnoremap <silent> <leader>qa :quitall<cr>
nnoremap <silent> <leader>wa :writeall<cr>

" delete
nnoremap <silent> x "_x
nnoremap <silent> <leader>x x

nnoremap <silent> d "_d
nnoremap <silent> <leader>d d

" search
nnoremap <silent> <leader>n :%s///gn<cr>
nnoremap <silent> <c-n>     :nohlsearch<cr>

" paste
inoremap <silent> <f3> <c-o>:set nopaste<cr><c-r>+
inoremap <silent> <f4> <c-o>:set paste<cr><c-r>+<c-o>:set nopaste<cr>

" nerdtree
nnoremap <silent> <leader>ntt :NERDTreeToggle<cr>

" fzf
nnoremap <silent> <leader>fzf :FZF<cr>
nnoremap <silent> <leader>fzfw :Windows<cr>

" youcompleteme
let g:ycm_confirm_extra_conf = 0

" eclim
let g:EclimBrowser = 'tmux new-window elinks'
let g:EclimCompletionMethod = 'omnifunc'
let g:EclimProjectTreeActions = [ {'pattern': '.*', 'name': 'Edit', 'action': 'edit'} ]
let g:EclimJavaSearchSingleResult = 'edit'
let g:EclimJavaHierarchyDefaultAction = 'edit'
let g:EclimJavaCallHierarchyDefaultAction = 'edit'


nnoremap      <silent> <leader>ept    :ProjectTreeToggle<cr>

nnoremap      <silent> <leader>ejf    :JavaFormat<cr>
nnoremap      <silent> <leader>ejff   :%JavaFormat<cr>

nnoremap      <silent> <leader>eji    :JavaImport<cr>
nnoremap      <silent> <leader>ejio   :JavaImportOrganize<cr>
nnoremap      <silent> <leader>ejct   :JavaCorrect<cr>

nnoremap               <leader>ejn    :JavaNew<space>
nnoremap               <leader>ejnc   :JavaNew class<space>
nnoremap               <leader>ejna   :JavaNew abstract<space>
nnoremap               <leader>ejni   :JavaNew interface<space>
nnoremap               <leader>ejne   :JavaNew enum<space>
nnoremap               <leader>ejna   :JavaNew @interface<space>

nnoremap               <leader>ejr    :JavaRename<space>

nnoremap      <silent> <leader>ejc    :JavaConstructor<cr>
nnoremap      <silent> <leader>ejci   :JavaConstructor!<cr>

nnoremap      <silent> <leader>ejd    :JavaDelegate<cr>

nnoremap      <silent> <leader>ejg    :JavaGet<cr>
nnoremap      <silent> <leader>ejgi   :JavaGet!<cr>
nnoremap      <silent> <leader>ejs    :JavaSet<cr>
nnoremap      <silent> <leader>ejsi   :JavaSet!<cr>
nnoremap      <silent> <leader>ejgs   :JavaGetSet<cr>
nnoremap      <silent> <leader>ejgsi  :JavaGetSet!<cr>

nnoremap      <silent> <leader>ejrr   :JavaRefactorRedo<cr>
nnoremap      <silent> <leader>ejru   :JavaRefactorUndo<cr>

nnoremap      <silent> <leader>ejrrp  :JavaRefactorRedoPeek<cr>
nnoremap      <silent> <leader>ejrup  :JavaRefactorUndoPeek<cr>

nnoremap      <silent> <leader>ejh    :JavaHierarchy<cr>

nnoremap      <silent> <leader>ejht   :JavaHierarchy -a tabnew<cr>
nnoremap      <silent> <leader>ejhs   :JavaHierarchy -a split<cr>
nnoremap      <silent> <leader>ejhv   :JavaHierarchy -a vsplit<cr>

nnoremap      <silent> <leader>ejch   :JavaCallHierarchy<cr>

nnoremap      <silent> <leader>ejcht  :JavaCallHierarchy -a tabnew<cr>
nnoremap      <silent> <leader>ejchs  :JavaCallHierarchy -a split<cr>
nnoremap      <silent> <leader>ejchv  :JavaCallHierarchy -a vsplit<cr>

nnoremap      <silent> <leader>ejchi  :JavaCallHierarchy!<cr>

nnoremap      <silent> <leader>ejchit :JavaCallHierarchy! -a tabnew<cr>
nnoremap      <silent> <leader>ejchis :JavaCallHierarchy! -a split<cr>
nnoremap      <silent> <leader>ejchiv :JavaCallHierarchy! -a vsplit<cr>

nnoremap      <silent> <leader>ejs    :JavaSearch<cr>
nnoremap      <silent> <leader>ejst   :JavaSearch -a tabnew<cr>
nnoremap      <silent> <leader>ejss   :JavaSearch -a split<cr>
nnoremap      <silent> <leader>ejsv   :JavaSearch -a vsplit<cr>

nnoremap      <silent> <leader>ejsc   :JavaSearchContext<cr>

nnoremap      <silent> <leader>ejsct  :JavaSearchContext -a tabnew<cr>
nnoremap      <silent> <leader>ejscs  :JavaSearchContext -a split<cr>
nnoremap      <silent> <leader>ejscv  :JavaSearchContext -a vsplit<cr>

nnoremap      <silent> <leader>ejdc   :JavaDocComment<cr>

nnoremap      <silent> <leader>ejdp   :JavaDocPreview<cr>
nnoremap      <silent> <leader>ejds   :JavaDocSearch<cr>

" youcompleteme
nnoremap     <silent> <leader>yc  :YcmCompleter GoToDeclaration<cr>
nnoremap     <silent> <leader>ycc :YcmCompleter GoToDefinition<cr>

" clang-format
noremap  <silent> <leader>cf           :pyf ~/.config/nvim/python/clang-format.py<cr>
inoremap <silent> <leader>cf      <c-o>:pyf ~/.config/nvim/python/clang-format.py<cr>
nnoremap <silent> <leader>cff          :%pyf ~/.config/nvim/python/clang-format.py<cr>

" gundo
nnoremap <silent> <leader>gut :GundoToggle<cr>

" fugitive
nnoremap <silent> <leader>fdgt   :diffget //2<cr>
nnoremap <silent> <leader>fdgm   :diffget //3<cr>

autocmd QuickFixCmdPost *grep* cwindow

" tasks
nnoremap <silent> <leader>td :Ag \(FIXME\)\\|\(TODO\)<cr>

" x2x
xmap <leader>xb  <plug>x2b
nmap <leader>xb  <plug>x2b
xmap <leader>xo  <plug>x2o
nmap <leader>xo  <plug>x2o
xmap <leader>xd  <plug>x2d
nmap <leader>xd  <plug>x2d
xmap <leader>xh  <plug>x2h
nmap <leader>xh  <plug>x2h
