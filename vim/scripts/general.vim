" General Settings{{{
filetype off
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
set cursorline

" Terminal Colors
set termguicolors

" General Tab settings {{{
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}


