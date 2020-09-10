" General Settings{{{
filetype off
" Syntax highlight
syntax on
" enable filetype plugin loading
filetype indent plugin on

" Case insensitive
set ignorecase
" ??
set modeline
" vi Incompatible defaults 
set nocompatible
" Default encoding to UTF-8
set encoding=utf-8
" Highlight search result in pattern search
set hlsearch
" Command history capacity
set history=700
" Maximum number of tabs to be opened
set tabpagemax=100
" Show column and line number in status bar
set ruler
" Enable mouse support
set mouse=a
" Enable line numbers
set nu 
" Set <leader> to SPC
let mapleader = " "
" Disable line wrapping in long lines
set nowrap
" Start new line with same indent level as current line
set autoindent
noremap j gj
noremap k gk
" Highlight current line
set cursorline

" General Tab settings {{{
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}


call plug#begin('~/.vim/plugged')
" Themes {{{
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
Plug 'whatyouhide/vim-gotham'
" }}}

" tpope {{{
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
Plug 'tpope/vim-fugitive'
" }}}

Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'michaeljsmith/vim-indent-object'

" Languages {{{
Plug 'honza/dockerfile.vim'
Plug 'ziglang/zig.vim'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" }}}

" Autocomplete {{{
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'roxma/nvim-yarp'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'lighttiger2505/deoplete-vim-lsp'
Plug 'mattn/vim-lsp-settings'
" }}}

" Status bar {{{
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" }}}

call plug#end()

colorscheme gotham

" FileType specific settings {{{
augroup format
        " PHP
        autocmd Filetype php setlocal expandtab tabstop=4 shiftwidth=4 sts=4

        " Go
        autocmd Filetype go setlocal expandtab tabstop=4 softtabstop=4 shiftwidth=4
       
        " Python 
        autocmd Filetype python setlocal expandtab tabstop=4 softtabstop=4 shiftwidth=4

        " C/C++
        autocmd Filetype c,cpp setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2

        " JSON 
        autocmd Filetype json setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
	
        " YAML 
        autocmd Filetype yaml setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
augroup end
" }}}

"Easier window navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :
" }}}

" Disable highlight after search
nmap <leader>/ :nohl<CR>
nmap , :nohl<CR>

" Exit Insert mode more easily {{{
inoremap jk <esc>
inoremap kj <esc> 
" }}}

" Go {{{
let g:go_fmt_command = "goimports"
" }}}

" Make copy/pase normal
set clipboard=unnamedplus

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>v :GitFiles<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>w :Windows<CR>
nnoremap <leader>c :Commands<CR>
nnoremap <leader>s :Rg<CR>
map <C-s> :Rg<CR>
"}}}

" NetRW settings {{{
let g:netrw_banner = 0
let g:netrw_winsize = 20
let g:netrw_liststyle = 3
" }}}

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

augroup Git
	nnoremap <f1> :! git diff %<cr>
	nnoremap <f2> :! git add %<cr>
augroup end

augroup Go
	nnoremap <f5> :GoBuild<cr> 
	nnoremap <f6> :GoTest<cr>
augroup end



