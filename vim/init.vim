syntax on
set number
set encoding=utf-8

set hlsearch
set incsearch
set ignorecase
set smartcase

filetype plugin indent on

call plug#begin('~/.vim/plugged')
Plug 'dracula/vim', { 'as': 'dracula' }

"Go"
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

"Git wrapper"
Plug 'tpope/vim-fugitive'

"Status line"
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"Rust"
Plug 'racer-rust/vim-racer'
Plug 'rust-lang/rust.vim'

"Auto Complete"
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'crusoexia/vim-monokai'

call plug#end()

colorscheme dracula 

set termguicolors

"Deoplate"
let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('omni_patterns', { 'go': '[^. *\t]\.\w*' })


"Rust Config"
let g:racer_experimental_completer = 1
let g:racer_insert_paren = 1
let g:rustfmt_autosave = 1


