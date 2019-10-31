set number

syntax on

set nocompatible

filetype plugin on

set path+=**

set wildmenu

set tags=./tags,tags;

set wildignore+=**/node_modules/** 
set wildignore+=**/target/** 
set completeopt=longest,menuone
set laststatus=2
set ruler

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

command! MakeTags !ctags -R .

call plug#begin('~/.vim/plugged')

Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'

call plug#end()


"Rust"
let g:rustfmt_autosave = 1
let g:racer_experimental_completer = 1
"Golang"

