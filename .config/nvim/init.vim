" theme
colorscheme zenburn
set background=dark

" vundle
filetype off
set rtp+=/home/jason/.config/nvim/bundle/vundle
call vundle#rc()

Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
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
Plugin 'tpope/vim-sleuth'
Plugin 'rking/ag.vim'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
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
set foldmethod=indent
set switchbuf=usetab,split
syntax on

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

"search
nnoremap <silent> <leader>n :%s///gn<cr>
nnoremap <silent> <c-n>     :nohlsearch<cr>

" paste
inoremap <silent> <f3> <c-o>:set nopaste<cr><c-r>+
inoremap <silent> <f4> <c-o>:set paste<cr><c-r>+<c-o>:set nopaste<cr>

"nerdtree
nnoremap <silent> <leader>ntt :NERDTreeToggle<cr>

" eclim
let g:EclimBrowser = '/usr/bin/chromium'
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
nnoremap      <silent> <leader>ejc    :JavaCorrect<cr>

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

nnoremap      <silent> <leader>ejds   :JavaDocSearch<cr>

nnoremap      <silent> <leader>ejdst  :JavaDocSearch -a tabnew<cr>
nnoremap      <silent> <leader>ejdss  :JavaDocSearch -a split<cr>
nnoremap      <silent> <leader>ejdsv  :JavaDocSearch -a vsplit<cr>

" youcompleteme
nnoremap     <silent> <leader>yc  :YcmCompleter GoToDeclaration<cr>
nnoremap     <silent> <leader>ycc :YcmCompleter GoToDefinition<cr>

" clang-format
noremap  <silent> <leader>cf           :pyf ~/.config/nvim/python/clang-format.py<cr>
inoremap <silent> <leader>cf      <c-o>:pyf ~/.config/nvim/python/clang-format.py<cr>
nnoremap <silent> <leader>cff          :%pyf ~/.config/nvim/python/clang-format.py<cr>

" gundo
nnoremap <silent> <leader>gg :GundoToggle<cr>

" fugitive
nnoremap <silent> <leader>fdgt   :diffget //2<cr>
nnoremap <silent> <leader>fdgm   :diffget //3<cr>

autocmd QuickFixCmdPost *grep* cwindow

" tasks
nnoremap <silent> <leader>t :Ag \(FIXME\)\\|\(TODO\)<cr>

" x2x
xnoremap <leader>xb  <plug>x2b
nnoremap <leader>xb  <plug>x2b
xnoremap <leader>xo  <plug>x2o
nnoremap <leader>xo  <plug>x2o
xnoremap <leader>xd  <plug>x2d
nnoremap <leader>xd  <plug>x2d
xnoremap <leader>xh  <plug>x2h
nnoremap <leader>xh  <plug>x2h
