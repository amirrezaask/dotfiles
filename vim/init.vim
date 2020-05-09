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
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
" Plug 'ncm2/ncm2'
" Plug 'roxma/nvim-yarp'
" Plug 'ncm2/ncm2-bufword'
" Plug 'ncm2/ncm2-path'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'StanAngeloff/php.vim'
Plug 'arcticicestudio/nord-vim'
call plug#end()

" General Settings{{{
syntax on
filetype off          " necessary to make ftdetect work on Linux
syntax on
filetype on           " Enable filetype detection
filetype indent on    " Enable filetype-specific indenting
filetype plugin on    " Enable filetype-specific plugins
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
colorscheme xoria256 
set relativenumber
set termguicolors
let mapleader = " "

"Easier window navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :
" }}}
nmap <leader>/ :nohl<CR>

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

" NCM2 settings
" set shortmess+=c
" inoremap <c-c> <ESC>
" autocmd BufEnter * call ncm2#enable_for_buffer()
" set completeopt=noinsert,menuone,noselect
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" inoremap <expr> <CR> (pumvisible() ? "\<c-y>" : "\<CR>")
" nnoremap <leader>. :LspDefinition<CR>
" LSP settings
source ~/.config/nvim/coc.vim
