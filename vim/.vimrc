set number

syntax on

set nocompatible

filetype plugin on

set path+=**

set wildmenu

set tags=./tags,tags;

set wildignore+=**/node_modules/** 
set wildignore+=**/target/** 
command! MakeTags !ctags -R .


call plug#begin('~/.vim/plugged')

Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

call plug#end()


colorscheme dracula

"Rust"
let g:rustfmt_autosave = 1

"Golang"
"
