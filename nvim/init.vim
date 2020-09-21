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
Plug 'neovim/nvim-lspconfig'

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
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" }}}

call plug#end()

" End Plugins

" define SPC as <leader> 
let mapleader=" "

" FZF
nnoremap <leader><leader> :Files<CR>
nnoremap \\ :Rg<CR> 
nnoremap // :BLines<CR>
nnoremap ?? :Rg<CR>
let g:fzf_layout = { 'window': { 'width': 0.95, 'height': 0.95 } }

" Colorscheme
colorscheme atom-dark 


