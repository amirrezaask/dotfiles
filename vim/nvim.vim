"Effient Over Fancy"
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'fatih/vim-go' 
Plug 'ekalinin/dockerfile.vim'
Plug 'tpope/vim-fugitive'
Plug 'michaeljsmith/vim-indent-object'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'ryanolsonx/vim-lsp-python'
Plug 'pearofducks/ansible-vim'
Plug 'dense-analysis/ale'
Plug 'airblade/vim-gitgutter'
Plug 'terryma/vim-multiple-cursors'
call plug#end()



"Basic setup"
set nocompatible
set number
set belloff=all
set encoding=utf-8
set guioptions-=m  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
set showcmd
set guifont=JetbrainsMono\ 13
filetype indent on
filetype plugin on
set wildmenu
set showmatch
set ignorecase
set relativenumber
set incsearch
set hlsearch
set foldenable
set foldlevelstart=10
set foldmethod=indent
set path=./
set path+=**
set ruler
set laststatus=2
set autowrite
set clipboard=unnamedplus
"Basic setup ends here"
au User lsp_setup call lsp#register_server({
    \ 'name': 'intelephense',
    \ 'cmd': {server_info->['node', expand('/usr/lib/node_modules/intelephense/lib/intelephense.js'), '--stdio']},
    \ 'initialization_options': {"storagePath": "/usr/lib/node_modules/intelephense"},
    \ 'whitelist': ['php'],
    \ 'workspace_config': { 'intelephense': {
    \   'files.associations': ['*.php'],
    \ }},
    \ })

"Keybidings"
let mapleader="\<SPACE>"
nnoremap <Leader>sc <C-W>o
nnoremap <Leader>ff :find 
nnoremap <Leader>sl <C-W><C-H>
nnoremap <Leader>sd <C-W><C-j>
nnoremap <Leader>su <C-W><C-k>
nnoremap <Leader>sr <C-W><C-l>
nnoremap <Leader>bs :w<CR>
nnoremap <Leader>ss :%s/
