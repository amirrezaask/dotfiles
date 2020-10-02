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


" Telescope {{{
    nnoremap <leader><leader> <cmd>lua require('telescope.builtin').find_files{}<CR>
    nnoremap <leader>ff <cmd>lua require('telescope.builtin').git_files{}<CR>
    nnoremap <leader>fb <cmd>lua require'telescope.builtin'.buffers()<CR>
    nnoremap <leader>fs <cmd>lua require'telescope.builtin'.lsp_workspace_symbols()<CR>
    nnoremap ?? <cmd>lua require('telescope.builtin').live_grep{}<CR>
" }}} 

" FZF {{{

    " nnoremap <leader><leader> :Files<CR>
    " nnoremap \\ :BLines<CR>
    " nnoremap ?? :Rg<CR>
    " let g:fzf_layout = { 'window': { 'width': 0.90, 'height': 0.90 } }
    " let g:fzf_preview_window = 'right:40%'

" }}}

" no backup files
set nobackup
set nowritebackup
set noswapfile

" Netrw configuration {{{
    map <f8> :Vex<CR>
    let g:netrw_liststyle = 3
    let g:netrw_banner = 0
    let g:netrw_browse_split = 1
    let g:netrw_winsize = 15 
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

if has('nvim')
    set splitbelow
    set splitright
endif

" edit configuration {{{
    let g:config_location = "~/w/dotfiles"
    command! Config lua require'telescope.builtin'.find_files{cwd=vim.api.nvim_get_var('config_location')}
    map <f9> :Config<CR>
" }}}

" Tabs {{{
    nnoremap tn :tabnext<CR>
    nnoremap tp :tabprevious<CR>
    nnoremap tc :tabclose<CR>
    nnoremap tt :tabnew<CR>
" }}}

" Enable go.nvim save hooks
let g:go_on_save_hooks = 1
