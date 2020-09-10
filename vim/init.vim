let $MYRTP="~/w/dotfiles/vim/rtp"
set runtimepath+=$MYRTP

source $MYRTP/general.vim
source $MYRTP/plugins.vim

colorscheme gotham

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
" Disable highlight after search
nmap <leader>/ :nohl<CR>
nmap , :nohl<CR>

" Exit Insert mode more easily {{{
inoremap jk <esc>
inoremap kj <esc> 
" }}}

" Go {{{
let g:go_fmt_command = "goimports"
" }}}

" Make copy/pase normal
set clipboard=unnamedplus

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

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}

augroup Git
	nnoremap <f1> :! git diff %<cr>
	nnoremap <f2> :! git add %<cr>
augroup end

augroup Go
	nnoremap <f5> :GoBuild<cr> 
	nnoremap <f6> :GoTest<cr>
augroup end



