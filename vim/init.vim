call plug#begin('~/.vim/plugged')
Plug 'vim-scripts/xoria256.vim'
Plug 'iCyMind/NeoSolarized'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'ncm2/ncm2-vim-lsp'
Plug 'jiangmiao/auto-pairs'
Plug 'preservim/nerdtree'
Plug 'ziglang/zig.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'StanAngeloff/php.vim'
Plug 'arcticicestudio/nord-vim'
call plug#end()

" General Settings{{{
syntax on
filetype on
set expandtab
set bs=2
set tabstop=2
set autoindent
set smartindent
set ignorecase
set modeline
set nocompatible
set encoding=utf-8
set hlsearch
set history=700
set t_Co=256
set tabpagemax=100
set ruler
set nojoinspaces
set shiftround
set guifont=Hack\ 10
colorscheme nord 
set termguicolors
let mapleader = ","

"Easier window navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :
" }}}


" Easier buffer switch {{{
noremap <leader>p  :bp<cr>
noremap <leader>n :bn<cr>
" }}}

" Easier tab{{{ 
noremap <leader>tt :tabnext<CR>
noremap <leader>ta :tabnew
noremap <leader>tc :tabclose<CR>
" }}}

" Exit Insert mode more easily {{{
inoremap jk <esc>
inoremap kj <esc> 
" }}}

" Make copy/pase normal
set clipboard=unnamedplus

" Edit neovim configuration instantly
nnoremap <leader>ec :tabnew ~/.config/nvim/init.vim<CR>   

" Fzf keybindings
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>w :Windows<CR>
nnoremap <leader>c :Commands<CR>
nnoremap <leader>g :Rg<CR>

" NCM2 settings
set shortmess+=c
inoremap <c-c> <ESC>
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <CR> (pumvisible() ? "\<c-y>" : "\<CR>")
nnoremap <leader>. :LspDefinition<CR>
