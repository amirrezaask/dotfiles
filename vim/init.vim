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
        autocmd Filetype yaml,yml setlocal expandtab tabstop=2 softtabstop=2 shiftwidth=2
augroup end
" }}}

" Go {{{
let g:go_fmt_command = "goimports"
" }}}

" Fzf keybinding
nnoremap <leader><leader> :Files<CR>
"}}}

" Deoplete {{{
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" }}}


