let $VIMSCRIPTS="$HOME/w/dotfiles/vim"

call plug#begin('~/.vim/plugged')
source $VIMSCRIPTS/plugins.vim
call plug#end()

" Basic setup
source $VIMSCRIPTS/base.vim

" Enable Deoplete completion
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" FZF to SPC SPC
nnoremap <leader><leader> :Files<CR>

" Colorscheme
colorscheme gotham

