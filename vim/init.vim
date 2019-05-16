set nocompatible

filetype off



call plug#begin('~/.local/share/nvim/plugged')



Plug 'vim-scripts/indentpython.vim'

Plug 'Valloric/YouCompleteMe'

Plug 'vim-syntastic/syntastic'

Plug 'nvie/vim-flake8'

Plug 'scrooloose/nerdtree'

Plug 'jistr/vim-nerdtree-tabs'

Plug 'kien/ctrlp.vim'

Plug 'tpope/vim-fugitive'

Plug 'rafi/awesome-vim-colorschemes'

Plug 'fatih/vim-go'

Plug 'davidhalter/jedi-vim'

Plug 'SirVer/ultisnips'

Plug 'Shougo/deoplete.nvim'

Plug 'zchee/deoplete-go', { 'do': 'make'}

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

Plug 'junegunn/fzf.vim'

Plug 'itchyny/lightline.vim'



Plug 'terryma/vim-multiple-cursors'

Plug 'tpope/vim-eunuch'

Plug 'airblade/vim-gitgutter'

Plug 'chriskempson/base16-vim'

Plug 'markvincze/panda-vim'

call plug#end()



color onedark

set background=dark

filetype plugin indent on

set encoding=utf-8

let python_highlight_all=1

syntax on

set nu





"Python"
"
"au BufNewFile,BufRead *.py
"
"    \ set tabstop=4 |
"
"        \ set softtabstop=4 |
"
"            \ set shiftwidth=4 |
"
"                \ set textwidth=79 |
"
"                    \ set expandtab |
"
"                        \ set autoindent | 
"
"                            \ set fileformat=unix |
"
"
"
"                            let g:jedi#goto_definitions_command = "<C-d>"
"
"
"
"                            let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore
"                            files in NERDTree
"
"
"
"                            au BufNewFile,BufRead *.js, *.html, *.css
"
"                                \ set tabstop=2
"
"                                    \ set softtabstop=2
"
"                                        \ set shiftwidth=2
"
"
"
"
"
"                                        "GO"
"
"                                        let g:go_auto_type_info = 1
"
"                                        let g:go_test_timeout = '30s'
"
"                                        let g:go_highlight_build_constraints
"                                        = 1
"
"                                        let g:go_highlight_extra_types = 1
"
"                                        let g:go_highlight_fields = 1
"
"                                        let g:go_highlight_functions = 1
"
"                                        let g:go_highlight_methods = 1
"
"                                        let g:go_highlight_operators = 1
"
"                                        let g:go_highlight_structs = 1
"
"                                        let g:go_highlight_types = 1
"
"                                        let g:go_auto_sameids = 1
"
"                                        let g:go_fmt_command = "goimports"
"
"                                        let g:deoplete#enable_at_startup = 1
"
"                                        let
"                                        g:deoplete#sources#go#gocode_binary =
"                                        '/home/amirreza/go/bin/gocode'
"
"                                        au FileType go nmap <C-d>
"                                        <Plug>(go-def)
"
"
"
"                                        set updatetime=50
"
"
"
"                                        autocmd StdinReadPre * let s:std_in=1
"
"
"
"
"
"                                        autocmd VimEnter * if argc() == 0 &&
"                                        !exists("s:std_in") | NERDTree |
"                                        endif
"
"
"
"                                        map <C-n> :NERDTreeToggle<CR>
"
"                                        map <C-e> :GoTestFunc<CR>
"
"                                        map <C-r> :GoRun<CR>
"
"                                        map <C-x><C-o> <C-s>
"
"                                        let
"                                        g:multi_cursor_use_default_mapping=0
"
"
"
"                                        " Default mapping
"
"                                        let g:multi_cursor_start_word_key
"                                        = '<C-c>'
"
"                                        let
"                                        g:multi_cursor_select_all_word_key =
"                                        '<A-n>'
"
"                                        let g:multi_cursor_start_key
"                                        = 'g<C-n>'
"
"                                        let g:multi_cursor_select_all_key
"                                        = 'g<A-n>'
"
"                                        let g:multi_cursor_next_key
"                                        = '<C-c>'
"
"                                        let g:multi_cursor_prev_key
"                                        = '<C-p>'
"
"                                        let g:multi_cursor_skip_key
"                                        = '<C-x>'
"
"                                        let g:multi_cursor_quit_key
"                                        = '<Esc>'
