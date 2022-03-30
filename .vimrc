"     ___              _                            ___         __
"    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
"   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
"  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
" /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
"
" Vim Configuration for minimalist developer

call plug#begin()
    if has('nvim')
        Plug 'jghauser/mkdir.nvim'
        Plug 'windwp/nvim-spectre'
        Plug 'nvim-treesitter/nvim-treesitter' 
        Plug 'nvim-treesitter/nvim-treesitter-textobjects' 
        Plug 'hrsh7th/nvim-cmp' 
        Plug 'hrsh7th/cmp-buffer' 
        Plug 'hrsh7th/cmp-nvim-lua' 
        Plug 'hrsh7th/cmp-nvim-lsp' 
        Plug 'hrsh7th/cmp-path' 
        Plug 'neovim/nvim-lspconfig' 
        Plug 'luisiacc/gruvbox-baby'
        Plug 'lukas-reineke/indent-blankline.nvim' 
    else
        Plug 'prabirshrestha/asyncomplete.vim'
        Plug 'prabirshrestha/asyncomplete-lsp.vim'
        Plug 'prabirshrestha/vim-lsp'
        Plug 'mattn/vim-lsp-settings'
    endif
    Plug 'tpope/vim-surround'  
    Plug 'honza/dockerfile.vim' 
    Plug 'hashivim/vim-terraform' 
    Plug 'LnL7/vim-nix' 
    Plug 'dag/vim-fish' 
    Plug 'cespare/vim-toml' 
    Plug 'elixir-editors/vim-elixir' 
    Plug 'pearofducks/ansible-vim' 
    Plug 'Glench/Vim-Jinja2-Syntax' 
    Plug 'ziglang/zig.vim' 
    Plug 'rust-lang/rust.vim'
    Plug 'fatih/vim-go' 
    Plug 'fladson/vim-kitty'
    Plug 'vim-erlang/vim-erlang-runtime'
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'
    Plug 'eemed/sitruuna.vim'
    Plug 'ap/vim-buftabline'
    Plug 'tpope/vim-commentary'
    Plug 'dracula/vim'
call plug#end()


let g:mapleader = ' '

" Keymappings
nnoremap Q <NOP>
nnoremap ; :
nnoremap q; q:

nnoremap <Left> :vertical resize -5<CR>
nnoremap <Right> :vertical resize +5<CR>
nnoremap <Up> :resize +5<CR>
nnoremap <Down> :resize -5<CR>

nnoremap j gj
nnoremap k gk

tnoremap <Esc> <C-\><C-n>
tnoremap jk <C-\><C-n>
tnoremap kj <C-\><C-n>

inoremap jk <esc>
inoremap kj <esc>

nnoremap Y y$
nnoremap n nzz
nnoremap N "Nzz

nnoremap <M-t> :tabnew<CR>
nnoremap <M-p> :tabprev<CR>
nnoremap <M-n> :tabnext<CR>

nnoremap <M-j> :m .+1<CR>==
nnoremap <M-k> :m .-2<CR>==

inoremap <M-j> <Esc>:m .+1<CR>==gi
inoremap <M-k> <Esc>:m .-2<CR>==gi

vnoremap <M-k> :m >+1<CR>gv=gv
vnoremap <M-k> <Esc>:m .-2<CR>==gi

nnoremap { :cprev<CR>
nnoremap } :cnext<CR>

nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() 

" Options
set smartcase 
set noequalalways
set modeline
set autoread
set nocompatible
set encoding=utf-8
set hlsearch
set history=700
set tabpagemax=100
set ruler
set mouse=a
set wrap
set autoindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set nobackup
set nowritebackup
set noswapfile
set splitright
set splitbelow
set nocursorline
set relativenumber " show relative line numbers
set number " show current line number
set showmode " show current vim mode down of the screen
set showcmd " show commands as they are being typed
set clipboard=unnamed
set hidden
set updatetime=100
set incsearch
set guioptions=egmrti
set gfn=JetBrainsMono\ Nerd\ Font\ Mono\ 10
set backspace=indent,eol,start
set complete-=i " don't search for all included files
set wildmenu
set wildoptions=tagfile
set list listchars=tab:»·,trail:·,nbsp:·

" Colorscheme
set termguicolors
colorscheme sitruuna

" Statusline
if has('nvim')
    set laststatus=3
else
    set laststatus=2
endif
set statusline=%q%w%h%r%m%f\ %=\ %l:%c:%L


" FZF stuff
let g:fzf_preview_window = {}
let g:fzf_layout = {'down': '40%' }
nnoremap <leader><leader> <cmd>Files<CR>
nnoremap <leader>fp <cmd>Files ~/.local/share/nvim/site/pack/packer<CR>
nnoremap <leader>ps <cmd>Files ~/src/gitlab.snapp.ir<CR>
nnoremap <leader>en <cmd>Files ~/.config/nvim<CR>
nnoremap ?? <cmd>Rg<CR>

" LSP stuff
if !has('nvim')
    function! s:on_lsp_buffer_enabled() abort
        setlocal omnifunc=lsp#complete
        setlocal signcolumn=yes
        if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
        nmap <buffer> gd <plug>(lsp-definition)
        nmap <buffer> gs <plug>(lsp-document-symbol-search)
        nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
        nmap <buffer> gr <plug>(lsp-references)
        nmap <buffer> gi <plug>(lsp-implementation)
        nmap <buffer> gt <plug>(lsp-type-definition)
        nmap <buffer> <leader>rn <plug>(lsp-rename)
        nmap <buffer> [g <plug>(lsp-previous-diagnostic)
        nmap <buffer> ]g <plug>(lsp-next-diagnostic)
        nmap <buffer> K <plug>(lsp-hover)
        inoremap <buffer> <expr><c-f> lsp#scroll(+4)
        inoremap <buffer> <expr><c-d> lsp#scroll(-4)

        let g:lsp_format_sync_timeout = 1000
        autocmd! BufWritePre *.rs,*.go.,*.zig call execute('LspDocumentFormatSync')
        " refer to doc to add more commands
    endfunction

    " Setting up language servers
    if executable('zls')
        au User lsp_setup call lsp#register_server({
            \ 'name': 'zls',
            \ 'cmd': {server_info->['zls']},
            \ 'allowlist': ['zig'],
            \ })
    endif
    inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"
    autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
    augroup lsp_install
        au!
        " call s:on_lsp_buffer_enabled only for languages that has the server registered.
        autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
    augroup END
endif

augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
augroup end

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

let g:netrw_browse_split = 0
let g:netrw_banner = 0
let g:netrw_winsize = 25

" Golang
let g:go_fmt_autosave = 1
let g:go_imports_autosave = 1

" Yaml
autocmd FileType yaml setlocal cursorcolumn

" Zig
let g:zig_fmt_autosave = 1

