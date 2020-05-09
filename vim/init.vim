call plug#begin('~/.vim/plugged')
Plug 'vim-scripts/xoria256.vim'
Plug 'iCyMind/NeoSolarized'
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
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'airblade/vim-gitgutter'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'StanAngeloff/php.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
call plug#end()

" General Settings{{{
filetype off
syntax on
filetype on
filetype indent on
filetype plugin on
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
set mouse=a
set guifont=Hack\ 9 
colorscheme gruvbox 
set relativenumber
set termguicolors
let mapleader = " "
" }}}
" FileType specific settings {{{
augroup format
        " PHP
        autocmd Filetype php setlocal ts=4 sts=4 sw=4

	" Go
        autocmd Filetype go setlocal ts=4 sts=4 sw=4
       
	" Python 
        autocmd Filetype python setlocal ts=4 sts=4 sw=4

        " C
        autocmd Filetype c setlocal ts=2 sts=2 sw=2

	" JSON 
        autocmd Filetype json setlocal ts=2 sts=2 sw=2
	
	" YAML 
        autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
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

" Easier buffer switch {{{
noremap <leader>p :bp<cr>
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

" Source init.vim
nnoremap <leader>sv :so $MYVIMRC<CR>

" Make copy/pase normal
set clipboard=unnamedplus

" Edit neovim configuration instantly
nnoremap <leader>ec :tabnew ~/.config/nvim/init.vim<CR>   

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fgf :GitFiles<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fw :Windows<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fs :Rg<CR>
let g:airline_theme='base16_gruvbox_dark_hard'
" Coc settings {{{
source ~/.config/nvim/coc.vim
let g:coc_global_extensions = ['coc-go', 'coc-phpls', 'coc-python', "coc-vimlsp", "coc-json", "coc-yaml"]
" }}}
