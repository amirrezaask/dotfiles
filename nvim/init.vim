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

    " Colorschemes
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'overcache/NeoSolarized'
    Plug 'whatyouhide/vim-gotham'
    Plug 'gosukiwi/vim-atom-dark'
    Plug 'LnL7/vim-nix'

    " tpope {{{
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-jdaddy'
    Plug 'tpope/vim-commentary' 
    Plug 'tpope/vim-fugitive'
    " }}}

    " Deoplete {{{
    " Plug 'Shougo/deoplete.nvim'
    " Plug 'Shougo/deoplete-lsp'
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
    " Language Server Protocol {{{
    " Plug 'autozimu/LanguageClient-neovim', {
    "     \ 'branch': 'next',
    "     \ 'do': 'bash install.sh',
    "     \ }
    " }}}

    " FZF {{{
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
    Plug 'junegunn/fzf.vim'
    " }}}

    " Indent Objects
    Plug 'nathanaelkane/vim-indent-guides'
    Plug 'michaeljsmith/vim-indent-object'

    " Languages {{{
    Plug 'honza/dockerfile.vim'
    Plug 'ziglang/zig.vim'
    Plug 'dag/vim-fish'
    Plug 'rust-lang/rust.vim'
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
    " }}}

    " Status bar {{{
    Plug 'itchyny/lightline.vim'
    " }}}
    " Telescope {{{
    " Plug 'nvim-lua/popup.nvim'
    " Plug 'nvim-lua/plenary.nvim'
    " Plug 'nvim-lua/telescope.nvim'
    " }}}
call plug#end()
" End Plugins
" Telescope {{{

" nnoremap <leader><leader> <cmd>lua require('telescope.builtin').find_files{}<CR>
" nnoremap <leader>p <cmd>lua require('telescope.builtin').git_files{}<CR>
" nnoremap // <cmd>lua require('telescope.builtin').live_grep{}<CR>

" }}} 
colorscheme dracula



" FZF {{{
nnoremap <leader><leader> :Files<CR>
nnoremap // :BLines<CR>
nnoremap ?? :Rg<CR>
let g:fzf_layout = { 'window': { 'width': 0.90, 'height': 0.90 } }
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

" Neovim LSP Client Settings {{{
if has('nvim-0.5.0')
        nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
        nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
        nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
        nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
        nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
        nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
        nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
        nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
        nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>

lua << END
    local nvim_lsp = require('nvim_lsp')
    nvim_lsp.gopls.setup{}
    nvim_lsp.pyls.setup{}
    nvim_lsp.intelephense.setup{}
END

endif
" }}}
