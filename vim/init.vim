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
set relativenumber
set colorcolumn=80
"Leader Settings"
let mapleader = " "
" }}}
"Easier window navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :
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

" Packages {{{
call plug#begin('~/.vim/plugged')

" Colorschemes {{{ 
Plug 'altercation/vim-colors-solarized'
Plug 'vim-scripts/xoria256.vim'
" }}}
" Edit neovim configuration instantly
nnoremap <leader>ec :tabnew ~/.config/nvim/init.vim<CR>   

" FZF integration {{{
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>ww :Windows<CR>
nnoremap <leader>cc :Commands<CR>
nnoremap <leader>gg :Rg<CR>
" }}} 

" Git interface {{
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" }}}

" Side file tree browser {{{
Plug 'preservim/nerdtree'
" }}}

" Autocomplete {{{ 
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
set shortmess+=c
inoremap <c-c> <ESC>
" }}}

" Language server protocol {{{
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'ncm2/ncm2-vim-lsp'
nnoremap <leader>ld :LspDefinition<CR> 
nnoremap <leader>lp :bp<CR>
nnoremap <leader>lf :LspDocumentFormat<CR>
" }}}

" Syntax linting{{{
Plug 'dense-analysis/ale'
" }}}
" Zig {{{
Plug 'ziglang/zig.vim'
" }}}

" Golang settings {{{
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" }}}

" Some tpope plugins {{{
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
" }}}

" Airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()
" }}}
