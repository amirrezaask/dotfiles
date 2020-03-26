call plug#begin('~/.vim/plugged')
"Theme I use
Plug 'sjl/badwolf' 
"project fuzzy search
Plug 'ctrlpvim/ctrlp.vim'
"Extension to netrw
Plug 'tpope/vim-vinegar'
"JSON manipulation and pretty printing
Plug 'tpope/vim-jdaddy'
"Another beautiful theme
Plug 'dracula/vim', { 'as': 'dracula' }
"Change surrounding items like brackets or parens
Plug 'tpope/vim-surround'
"Comment operations
Plug 'tpope/vim-commentary'
"Golang development integration for vim
Plug 'fatih/vim-go' 
"Dockerfile syntax highlight
Plug 'ekalinin/dockerfile.vim'
"Define indents as Text Objects
Plug 'michaeljsmith/vim-indent-object'
"Ansible syntax highlighting
Plug 'pearofducks/ansible-vim'
"Show new/modified/deleted lines beside the line number
Plug 'airblade/vim-gitgutter'
"Install fzf
Plug 'junegunn/fzf', { 'do': './install --bin' }
"FZF vim support
Plug 'junegunn/fzf.vim'
"Beautiful fast status bar
Plug 'vim-airline/vim-airline'
"Some themes for status bar
Plug 'vim-airline/vim-airline-themes'
"Async jobs for vim
Plug 'prabirshrestha/async.vim'
"LSP client for vim
Plug 'prabirshrestha/vim-lsp'
"LSP client settings like registering popular langauges servers
Plug 'mattn/vim-lsp-settings'
"NCM completion manager for auto complete suggestions
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-vim-lsp'
" Dracula Theme
Plug 'dracula/vim', { 'as': 'dracula' }
"Vim side file manager
Plug 'preservim/nerdtree'

Plug 'yggdroot/indentline'
"Support for emojis
Plug 'junegunn/vim-emoji'
"Rust support
Plug 'rust-lang/rust.vim'
"Automatically insert pairs
Plug 'jiangmiao/auto-pairs'

call plug#end()

"Set theme"
colorscheme badwolf 
" Remap Q to nothing
nnoremap Q <NOP>
" We are using Vim not Vi
set nocompatible
" Show line number
set number
" Turn of bell sound for everything
set belloff=all
" Set default file encoding
set encoding=utf-8
" Show last command on the status line
set showcmd
" Support mouse
set mouse=a
" Filetype indents
filetype indent on
" Filetype plugins
filetype plugin on
" highlight current line
set cursorline
" Suggestions for commands on <TAB>
set wildmenu
set showmatch
set ignorecase
" relative line numbers useful vim commands
set relativenumber
set incsearch
set hlsearch
set foldenable
set foldlevelstart=10
set tabstop=4
set softtabstop=4
set smarttab
set shiftwidth=4
set expandtab
set foldmethod=indent
set path=./
set ruler
set laststatus=2
set autowrite
set clipboard=unnamedplus
"Better vertical movement for wrapped (long) lines"
nnoremap j gj
nnoremap k gk

"Keybidings"
let mapleader=","
nnoremap <Leader>ff :FZF<CR> 
nnoremap <Leader>wl :vsplit<CR>
nnoremap <Leader>wj :split<CR>
nnoremap <Leader>wm <C-w>o

"netrw tweaks"
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=0  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
set completeopt+=menuone,noselect,noinsert

au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent

autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

"NerdTree Setup"
map <F8> :NERDTreeToggle<CR>

"GitGutter setup"
let g:gitgutter_sign_added = emoji#for('small_blue_diamond')
let g:gitgutter_sign_modified = emoji#for('small_orange_diamond')
let g:gitgutter_sign_removed = emoji#for('small_red_triangle')
let g:gitgutter_sign_modified_removed = emoji#for('collision')

"NCM setup
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
set shortmess+=c
"With <Enter> close the pop up and create new line
inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

"folding setup 
set foldmethod=expr
  \ foldexpr=lsp#ui#vim#folding#foldexpr()
  \ foldtext=lsp#ui#vim#folding#foldtext()

"Auto complete setup"
autocmd CompleteDone * pclose
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" autocmd FileType go,python,php,javascript,html inoremap . .<C-x><C-o><C-n>

