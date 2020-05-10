call plug#begin('~/.vim/plugged')
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-vinegar'
Plug 'ziglang/zig.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'airblade/vim-gitgutter'
Plug 'morhetz/gruvbox'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'nathanaelkane/vim-indent-guides'
Plug 'michaeljsmith/vim-indent-object'
Plug 'junegunn/goyo.vim'
Plug 'honza/dockerfile.vim'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'neovim/nvim-lsp'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/deoplete-lsp'
call plug#end()


lua require'nvim_lsp'.gopls.setup{}
lua require'nvim_lsp'.vimls.setup{}
lua require'nvim_lsp'.pyls_ms.setup{}
lua require'nvim_lsp'.intelephense.setup{}

" General Settings{{{
filetype off
syntax on
filetype on
"filetype indent on
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
set cursorline
set nowrap
set autoindent
noremap j gj
noremap k gk
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
        autocmd Filetype yaml :IndentGuidesEnable 
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
nnoremap <leader>sc :so $MYVIMRC<CR>
" Make copy/pase normal
set clipboard=unnamedplus

" Edit neovim configuration instantly {{{
nnoremap <leader>ec :tabnew ~/.config/nvim/init.vim<CR>   
nnoremap <leader>es :tabnew ~/.config/fish/config.fish<CR>
" }}}

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fgf :GitFiles<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fw :Windows<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fs :Rg<CR>
let g:airline_theme='base16_gruvbox_dark_hard'
"}}}

" NERDTree Settings {{{
map <silent> <F8> :NERDTreeToggle<CR>
" }}}

"Neovim LSP settings {{{
nnoremap <silent> ''  <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> ';      <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> '"    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> '\ <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> '/   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> '.    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> '=    <cmd>lua vim.lsp.buf.document_symbol()<CR>
nnoremap <silent> '-    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
"}}}


" deoplete settings {{{

let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif  " Close Doc window after exiting the completion menu
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" }}}

" NetRW settings {{{
let g:netrw_banner = 0
let g:netrw_winsize = 20
let g:netrw_liststyle = 3
" }}}

