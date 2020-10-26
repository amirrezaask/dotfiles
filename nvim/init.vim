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

" Set <leader> to SPC
let mapleader = " "

" Disable line wrapping in long lines
set wrap 

" Start new line with same indent level as current line
set autoindent

noremap j gj
noremap k gk

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

" Luzzy.nvim {{{
    nnoremap <leader><leader> <cmd>lua require'fuzzy'.file_finder{depth=5}<CR>
    nnoremap <leader>fg <cmd>lua require('fuzzy').git_files{}<CR>
    nnoremap ?? <cmd>lua require('fuzzy').grep{depth=5}<CR>
    nnoremap <leader>b <cmd>lua require('fuzzy').buffers{}<CR>
    nnoremap ?g <cmd>lua require('fuzzy').git_grep{}<CR>
" }}}

" Find things {{{
    map <leader>ec <cmd>lua require'fuzzy'.file_finder{path='/home/amirreza/w/dotfiles'}<CR>
    map <leader>en <cmd>lua require'fuzzy'.file_finder{path='/home/amirreza/w/dotfiles/nvim'}<CR>
    map <leader>fnp <cmd>lua require'fuzzy'.cd{cwd="~/.local/share/nvim/site/pack/packer/start/"}<CR>
    map <leader>fp <cmd>lua requre'fuzzy'.cd{cwd="~/w"}<CR>
" }}}

" Runner {{{
    map <f5> :Run<CR>
" }}}

" Tabs {{{
    nnoremap tn :tabnext<CR>
    nnoremap tp :tabprevious<CR>
    nnoremap tc :tabclose<CR>
    nnoremap tt :tabnew<CR>
" }}}

" Netrw {{{
    let g:netrw_banner = 0
" }}}
set guifont=Ubuntu\ Mono\ 10
" Lua {{{
    autocmd BufEnter *.lua set ts=2 sw=2 sts=2 expandtab
" }}}
" LazyGit {{{
    nnoremap <leader>gg :LazyGit<CR>
    let g:lazygit_floating_window_scaling_factor = 0.7
" }}}

" Snippets {{{
    inoremap <c-k> <cmd>lua return require'snippets'.expand_or_advance(1)<CR>
    inoremap <c-j> <cmd>lua return require'snippets'.advance_snippet(-1)<CR>
" }}}

" GitMessager {{{
    nnoremap <leader>gm :GitMessenger<CR>
" }}}
