Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
Plug 'whatyouhide/vim-gotham'

" tpope {{{
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-commentary' 
" }}}

" NCM2 {{{ 
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
" }}}

" Language Server Protocol {{{
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
" }}}

" FZF {{{
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
" }}}
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



