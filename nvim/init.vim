function! s:SimpleConfiguration()
	set ignorecase
	set modeline
	set autoread
	set cursorline
	set guicursor
	set nocompatible
	set encoding="utf-8"
	set hlsearch
	set history = 700
	set tabpagemax=100
	set ruler
	set mouse=a
	set wrap
	set autoindent
	set termguicolors
	set tabstop=4
	set shiftwidth=4
	set softtabstop=4
	set expandtab
	set nobackup
	set nowritebackup
	set noswapfile
	set clipboard="unnamedplus"
	set splitright
	set splitbelow
endfunction


if (has('nvim'))
	lua require'init'
else
	s:SimpleConfiguration()
endif

