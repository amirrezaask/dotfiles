let $VIMSCRIPTS="$HOME/w/dotfiles/vim"

call plug#begin('~/.vim/plugged')
source $VIMSCRIPTS/plugins.vim
call plug#end()

" Basic setup
source $VIMSCRIPTS/base.vim

" FZF to SPC SPC
nnoremap <leader><leader> :Files<CR>
nnoremap \\ :Rg<CR> 
nnoremap ,, :Commands<CR>
" Colorscheme
colorscheme atom-dark 

" NCM2
source $VIMSCRIPTS/ncm2.vim

" LSP configuration
source $VIMSCRIPTS/lsp.vim

" Gvim configuration
set guioptions=

" Font configuration
set guifont=Iosevka\ 12
