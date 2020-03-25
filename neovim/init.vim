call plug#begin('~/.vim/plugged')
Plug 'sjl/badwolf' 
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-jdaddy'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'fatih/vim-go' 
Plug 'ekalinin/dockerfile.vim'
Plug 'michaeljsmith/vim-indent-object'
Plug 'pearofducks/ansible-vim'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'preservim/nerdtree'
Plug 'yggdroot/indentline'
Plug 'junegunn/vim-emoji'
Plug 'rust-lang/rust.vim'
Plug 'jiangmiao/auto-pairs'
call plug#end()

"Basic setup"
set termguicolors
colorscheme badwolf 
nnoremap Q <NOP>
set guifont=mononoki\ 9
set nocompatible
set number
set belloff=all
set encoding=utf-8
set showcmd
set mouse=a
filetype indent on
filetype plugin on
set cursorline
set wildmenu
set showmatch
set ignorecase
set relativenumber
set incsearch
set hlsearch
set foldenable
set foldlevelstart=10
set tabstop=4
set softtabstop=4
set smarttab
set shiftwidth=4
set expandtab
set foldmethod=indent
set path=./
set ruler
set laststatus=2
set autowrite
set clipboard=unnamedplus

"Keybidings"
let mapleader=","
nnoremap <Leader>ff :FZF<CR> 
nnoremap <Leader>wl :vsplit<CR>
nnoremap <Leader>wj :split<CR>
nnoremap <Leader>wm <C-w>o

"netrw tweaks"
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=0  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
set completeopt+=menuone,noselect,noinsert

au! BufNewFile,BufReadPost *.{yaml,yml} set filetype=yaml foldmethod=indent

autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

"NerdTree Setup"
map <F8> :NERDTreeToggle<CR>

"GitGutter setup"
let g:gitgutter_sign_added = emoji#for('small_blue_diamond')
let g:gitgutter_sign_modified = emoji#for('small_orange_diamond')
let g:gitgutter_sign_removed = emoji#for('small_red_triangle')
let g:gitgutter_sign_modified_removed = emoji#for('collision')

"LSP setup"
function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> <f2> <plug>(lsp-rename)
endfunction
augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
 
set foldmethod=expr
  \ foldexpr=lsp#ui#vim#folding#foldexpr()
  \ foldtext=lsp#ui#vim#folding#foldtext()

"Auto complete setup"
autocmd CompleteDone * pclose
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

autocmd FileType go,python,php,javascript,html inoremap . .<C-x><C-o><C-n>

