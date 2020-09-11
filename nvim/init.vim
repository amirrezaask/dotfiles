let $VIMSCRIPTS="$HOME/w/dotfiles/vim/rtp"
source $VIMSCRIPTS/plugin/plugins.vim 
let mapleader=" "

" FZF to SPC SPC
nnoremap <leader><leader> :Files<CR>
nnoremap \\ :Rg<CR> 
nnoremap ,, :Commands<CR>

" Colorscheme
colorscheme gotham 

" Gvim configuration
set guioptions=

" Font configuration
set guifont=Iosevka\ 12
