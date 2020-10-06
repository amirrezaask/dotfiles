syntax on
" enable filetype plugin loading
filetype indent plugin on

" Case insensitive
set ignorecase
" ??
set modeline
set autoread
" Highlight current line 
set cursorline

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

" Relative line numbers
set relativenumber
" Terminal Colors
set termguicolors

" General Tab settings {{{
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}

" Install/Load plugins
lua require'plugins'

" no backup files
set nobackup
set nowritebackup
set noswapfile

" Fern {{{
map <f8> <cmd>Fern -toggle -drawer .<CR>
" }}}

"Easier window navigation {{{
    map <C-j> <C-w>j
    map <C-k> <C-w>k
    map <C-l> <C-w>l
    map <C-h> <C-w>h
" }}}

map Q <nop>

" Map ; to : for simpler command execuation
map ; :

" Disable highlight after search
nmap <leader>/ :nohl<CR>

" Exit Insert mode more easily {{{
    inoremap jk <esc>
    inoremap kj <esc> 
    inoremap kk <esc>
    inoremap jj <esc>
" }}}

" statusline {{{
let g:enable_express_line = 1
" }}}

" Make copy/paste from system clipboard normal
set clipboard=unnamedplus

set splitbelow
set splitright

" fuzzy.nvim {{
    nnoremap <leader>c <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').cd{}<CR>
    nnoremap <leader><leader> <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').files{}<CR>
    nnoremap <leader>fg <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').git_files{}<CR>
    nnoremap ?? <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').rg{}<CR>
    nnoremap ?g <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').git_grep{}<CR>
" }}

" Find things {{{
    let g:config_location = "~/w/dotfiles"
    map <leader>ec <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').files{cwd=vim.g.config_location}<CR>
    map <leader>ea <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').files{cwd='~/.config/awesome'}<CR>
    map <leader>en <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').files{cwd='~/.config/nvim/'}<CR>

    let g:plugins_location = "~/.local/share/nvim/site/pack/packer/start/"
    let g:projects_location = "~/w"
    map <leader>fnp <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').cd{cwd=vim.g.plugins_location}<CR>
    map <leader>fp <cmd>lua require('fuzzy.builtin')(require'fuzzy.fzf').cd{cwd=vim.g.projects_location}<CR>
" }}}

" Tabs {{{
    nnoremap tn :tabnext<CR>
    nnoremap tp :tabprevious<CR>
    nnoremap tc :tabclose<CR>
    nnoremap tt :tabnew<CR>
" }}}

" Enable go.nvim save hooks
" let g:go_disable_save_hooks = 1
" let g:go_formatter = "gofmt"
