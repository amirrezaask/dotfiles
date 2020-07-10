call plug#begin('~/.vim/plugged')
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-vinegar'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
Plug 'tpope/vim-fugitive'
Plug 'junegunn/gv.vim'
Plug 'airblade/vim-gitgutter'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'michaeljsmith/vim-indent-object'
" Languages {{{
Plug 'honza/dockerfile.vim'
Plug 'ziglang/zig.vim'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
" }}}
Plug 'chriskempson/base16-vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'lighttiger2505/deoplete-vim-lsp'
Plug 'mattn/vim-lsp-settings'
call plug#end()

" General Settings{{{
filetype off
syntax on
filetype on
filetype indent on
filetype plugin on
set ignorecase
set modeline
set nocompatible
set encoding=utf-8
set hlsearch
set history=700
set t_Co=256
set tabpagemax=100
set ruler
set nojoinspaces
set shiftround
set mouse=a
set guifont=Jetbrains\ Mono\ 10
colorscheme base16-monokai 
set termguicolors
set nu 
let mapleader = " "
set cursorline
set nowrap
set autoindent
noremap j gj
noremap k gk
" }}}
" FileType specific settings {{{
augroup format
        " PHP
        autocmd Filetype php setlocal expandtab tabstop=4 shiftwidth=4 sts=4

	" Go
        autocmd Filetype go setlocal ts=4 sts=4 sw=4
       
	" Python 
        autocmd Filetype python setlocal ts=4 sts=4 sw=4

        " C
        autocmd Filetype c setlocal ts=2 sts=2 sw=2

	" JSON 
        autocmd Filetype json setlocal ts=2 sts=2 sw=2
	
	" YAML 
        autocmd Filetype yaml setlocal ts=2 sts=2 sw=2
        autocmd Filetype yaml :IndentGuidesEnable 
augroup end
" }}}

"Easier window navigation {{{
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h
map Q <nop>
map ; :
" }}}

" Disable highlight after search
nmap <leader>/ :nohl<CR>

" Easier buffer switch {{{
noremap <leader>p :bp<cr>
noremap <leader>n :bn<cr>
" }}}

" Easier tab{{{ 
noremap <leader>tt :tabnext<CR>
noremap <leader>ta :tabnew
nnoremap <leader>tp :tabprevious<CR>
nnoremap <leader>tn :tabprevious<CR>
noremap <leader>tc :tabclose<CR>
" }}}

" Exit Insert mode more easily {{{
inoremap jk <esc>
inoremap kj <esc> 
" }}}

" Source this 
nnoremap <leader>so :so %<CR>

" Make copy/pase normal
set clipboard=unnamedplus

" Edit neovim configuration instantly {{{
nnoremap <leader>ec :tabnew ~/.config/nvim/init.vim<CR>   
nnoremap <leader>es :tabnew ~/.config/fish/config.fish<CR>
" }}}

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>ff :Files<CR>
nnoremap <leader>fgf :GitFiles<CR>
nnoremap <leader>fb :Buffers<CR>
nnoremap <leader>fw :Windows<CR>
nnoremap <leader>fc :Commands<CR>
nnoremap <leader>fs :Rg<CR>
"}}}

" NetRW settings {{{
let g:netrw_banner = 0
let g:netrw_winsize = 20
let g:netrw_liststyle = 3
" }}}

function! ToggleColorMode()
	if &g:background == 'dark'
		let &g:background="light"
	elseif &g:background == 'light'
		let &g:background= "dark"
	endif
endfunction
nnoremap <f12> :call ToggleColorMode()<CR>
" Deoplete {{{
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25
let g:NetrwIsOpen=0

function! ToggleNetrw()
    if g:NetrwIsOpen
        let i = bufnr("$")
        while (i >= 1)
            if (getbufvar(i, "&filetype") == "netrw")
                silent exe "bwipeout " . i 
            endif
            let i-=1
        endwhile
        let g:NetrwIsOpen=0
    else
        let g:NetrwIsOpen=1
        silent Lexplore
    endif
endfunction
map <f8> :call ToggleNetrw()<cr>

