call plug#begin('~/.vim/plugged')
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'overcache/NeoSolarized'
Plug 'whatyouhide/vim-gotham'

" tpope {{{
Plug 'tpope/vim-surround'
Plug 'tpope/vim-jdaddy'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-commentary' 
Plug 'tpope/vim-fugitive'
" }}}

Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-vinegar'
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
set termguicolors
set tabpagemax=100
set ruler
set nojoinspaces
set shiftround
set mouse=a
set guifont=Fira\ Mono\ 11
set nu 
colorscheme gotham 
let mapleader = " "
set nowrap
set autoindent
noremap j gj
noremap k gk
set cursorline
" General Tab settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" }}}

" FileType specific settings {{{
augroup format
        " PHP
        autocmd Filetype php setlocal expandtab tabstop=4 shiftwidth=4 sts=4

	" Go
        autocmd Filetype go setlocal expandtab tabstop=4 softtabstop=4 shiftwidth=4
       
	" Python 
        autocmd Filetype python setlocal expandtab tabstop=4 softtabstop=4 shiftwidth=4

        " C/C++
        autocmd Filetype c,cpp setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2

	" JSON 
        autocmd Filetype json setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
	
	" YAML 
        autocmd Filetype yaml setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
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
nmap , :nohl<CR>
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

" Go {{{
let g:go_fmt_command = "goimports"
" }}}
" Source this 
nnoremap <leader>so :so %<CR>

" Make copy/pase normal
set clipboard=unnamedplus

" Edit neovim configuration instantly {{{
nnoremap <f9> :tabnew ~/.config/nvim/init.vim<CR>   
nnoremap <f10> :tabnew ~/.config/fish/config.fish<CR>
" }}}

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
nnoremap <leader>f :Files<CR>
nnoremap <leader>v :GitFiles<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>w :Windows<CR>
nnoremap <leader>c :Commands<CR>
nnoremap <leader>s :Rg<CR>
map <C-s> :Rg<CR>
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

augroup Git
	nnoremap <f1> :! git diff %<cr>
	nnoremap <f2> :! git add %<cr>
augroup end

augroup Go
	nnoremap <f5> :GoBuild<cr> 
	nnoremap <f6> :GoTest<cr>
augroup end



