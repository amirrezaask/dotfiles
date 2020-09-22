" Syntax highlight
syntax on
" enable filetype plugin loading
filetype indent plugin on

" Case insensitive
set ignorecase
" ??
set modeline

" Highlight current line 
set cursorline

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
" set cursorline

" Relative line numbers
set relativenumber
" Terminal Colors
set termguicolors

" General Tab settings {{{
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}


" Plugins
call plug#begin('~/.vim/plugged')
    "Telescope
    if has('nvim')
        " Plug 'nvim-lua/popup.nvim'
        " Plug 'nvim-lua/plenary.nvim'
        " Plug 'nvim-lua/telescope.nvim'
    endif 
    " Colorschemes
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'overcache/NeoSolarized'
    Plug 'whatyouhide/vim-gotham'
    Plug 'arcticicestudio/nord-vim'
    Plug 'gosukiwi/vim-atom-dark'
    Plug 'morhetz/gruvbox'
    " tpope {{{
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-jdaddy'
    Plug 'tpope/vim-commentary' 
    Plug 'tpope/vim-fugitive'
    " }}}

    " File Manager
    Plug 'lambdalisue/fern.vim'
   
    " NeoVim LSP
    if has('nvim-0.5.0') 
        Plug 'neovim/nvim-lspconfig'
    endif
    if has('nvim')
        Plug 'vimlab/split-term.vim'
    endif

    " FZF {{{
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    " }}}

    " Indent Objects
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'michaeljsmith/vim-indent-object'

    " Languages {{{
    Plug 'honza/dockerfile.vim'
    Plug 'LnL7/vim-nix'
    Plug 'ziglang/zig.vim'
    Plug 'dag/vim-fish'
    Plug 'rust-lang/rust.vim'
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
    " }}}
    " Neovim completion {{{
    if has('nvim-0.5.0')
        Plug 'nvim-lua/completion-nvim'
        Plug 'tjdevries/nlua.nvim'
    endif
    
    if has('nvim')
        Plug 'nvim-lua/plenary.nvim'
        Plug 'tjdevries/express_line.nvim'
    endif
    " }}}
call plug#end()
" End Plugins

" Telescope {{{

" nnoremap <leader><leader> <cmd>lua require('telescope.builtin').find_files{}<CR>
" nnoremap <leader>ff <cmd>lua require('telescope.builtin').git_files{}<CR>
" nnoremap // <cmd>lua require('telescope.builtin').live_grep{}<CR>

" }}} 

colorscheme nord 

" FZF {{{

nnoremap <leader><leader> :Files<CR>
nnoremap \\ :BLines<CR>
nnoremap ?? :Rg<CR>
let g:fzf_layout = { 'window': { 'width': 0.90, 'height': 0.90 } }
let g:fzf_preview_window = 'right:40%'

" }}}

" no backup files
set nobackup
set nowritebackup
set noswapfile

" Project Explorer
map <f8> :Fern . -drawer -toggle<CR>
"Easier window navigation {{{
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l
    map <C-h> <C-w>h
" }}}

map Q <nop>

" Map ; to : for simpler command execuation
map ; :

" Disable highlight after search
nmap <leader>/ :nohl<CR>

" Exit Insert mode more easily {{{
    inoremap jk <esc>
    inoremap kj <esc> 
    inoremap kk <esc>
    inoremap jj <esc>
" }}}

" Make copy/paste from system clipboard normal
set clipboard=unnamedplus

if has('nvim')
    set splitbelow
    set splitright
endif

" edit configuration
let g:config_location = "~/w/dotfiles"
command! Config FZF ~/w/dotfiles 
map <f9> :Config<CR>

