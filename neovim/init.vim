" General Settings {{{

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
" set cursorline
" to vim let us put the cursor after the last char of the line
set ve+=onemore
" Suggestions for commands on <TAB>
set wildmenu
" show matching bracket
set showmatch
" ignore case (case insensitive ) when searching
set ignorecase
" relative line numbers useful vim commands
set relativenumber
" when searching show the matching words as you type 
set incsearch
" highlight search results
set hlsearch
" TODO: Need research
set foldenable
set foldlevelstart=10
set tabstop=4
set softtabstop=4
set smarttab
set shiftwidth=4
set expandtab
set foldmethod=indent
"set path+=**
set ruler
set laststatus=2
set autowrite
set clipboard=unnamedplus
let mapleader=" "

"Better vertical movement for wrapped (long) lines"
nnoremap j gj
nnoremap k gk
nnoremap ; :
nnoremap <leader>h :noh<cr>
" }}}


" Plugins {{{ 
call plug#begin('~/.vim/plugged')

"Extension to netrw
Plug 'tpope/vim-vinegar'
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=0  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

"Beautiful fast status bar
Plug 'itchyny/lightline.vim'

"JSON manipulation and pretty printing
Plug 'tpope/vim-jdaddy'

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
let g:gitgutter_sign_added = '+' 
let g:gitgutter_sign_modified = '~' 
let g:gitgutter_sign_removed = '-'
let g:gitgutter_sign_modified_removed = '~-'


"Install fzf
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
nnoremap :fzf :FZF<CR>
nnoremap <Leader>ff :FZF<CR> 

"NCM completion manager for auto complete suggestions
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

autocmd BufEnter * call ncm2#enable_for_buffer()

set completeopt=noinsert,menuone,noselect

Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
set shortmess+=c

" CTRL-C doesn't trigger the InsertLeave autocmd . map to <ESC> instead.
inoremap <c-c> <ESC>

" Use <TAB> to select the popup menu:
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" ncm2 source for vim-lsp
Plug 'ncm2/ncm2-vim-lsp'
" Async jobs for vim
Plug 'prabirshrestha/async.vim'
"LSP client for vim
Plug 'prabirshrestha/vim-lsp'
"LSP client settings like registering popular langauges servers
Plug 'mattn/vim-lsp-settings'
" Display indentation level
Plug 'yggdroot/indentline'
"Support for emojis
Plug 'junegunn/vim-emoji'
"Rust support
Plug 'rust-lang/rust.vim'
"Automatically insert pairs
Plug 'jiangmiao/auto-pairs'
call plug#end()
" }}}

" Filetype specific settings {{{
" YAML {{{
au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
"}}}

