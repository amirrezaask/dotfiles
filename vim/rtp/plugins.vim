call plug#begin('~/.vim/plugged')
" Themes {{{
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
Plug 'whatyouhide/vim-gotham' " }}}

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


