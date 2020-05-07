" General Settings
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
set guifont=Jetbrains\ Mono\ 11
colorscheme desert

"Leader Settings"
let mapleader = " "

"Easier window navigation
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :

" Exit Insert mode more easily
inoremap jk <esc>
inoremap kj <esc>

" Make copy/pase normal
set clipboard=unnamedplus
call plug#begin('~/.vim/plugged')

" FZF integration {{{
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
nnoremap <leader>ff :FZF<CR>
" }}} 

" Git Gutter 
Plug 'airblade/vim-gitgutter'

" Autocomplete {{{ 

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" }}}

" Language server protocol {{{
"
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'ncm2/ncm2-vim-lsp'
" }}}

" Zig
Plug 'ziglang/zig.vim'

" Golang settings
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
call plug#end()
