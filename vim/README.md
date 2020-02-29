# Vim Configuration
I use neovim so maybe some things are not fully compatible with vim itself, I didn't notice anything myself but jut FYI.
# Installation
```bash
git clone https://github.com/amirrezaask/dotfiles
mkdir -p .config/nvim
# for neovim
ln -s ./dofiles/vim/vimrc ~/.config/nvim/init.vim
# for vim
ln -s ./dotfiles/vim/vimrc ~/.vimrc
```
# Plugins
* [tpope/vim-vinegar](https://github.com/tpope/vim-vinegar)
* [tpope/vim-jdady](https://github.com/tpope/vim-jdaddy)
* [tpope/vim-surround](https://github.com/tpope/vim-surround)
* [tpope/vim-commentary](https://github.com/tpope/vim-commentary)
* [fatih/vim-go](https://github.com/fatih/vim-go)
* [ekalinin/dockerfile.vim](https://github.com/ekalinin/Dockerfile.vim)
* [michaeljsmith/vim-indent-object](https://github.com/michaeljsmith/vim-indent-object)
* [prabirshrestha/async.vim](https://github.com/prabirshrestha/async.vim)
* [prabirshrestha/vim-lsp](https://github.com/prabirshrestha/vim-lsp)
* [ryanolsonx/vim-lsp-python](https://github.com/ryanolsonx/vim-lsp-python)
* [pearofducks/ansible-vim](https://github.com/pearofducks/ansible-vim)
* [airblade/vim-gitgutter](https://github.com/airblade/vim-gitgutter)
* [junegunn/fzf](https://github.com/junegunn/fzf.vim)
* [junegunn/fzf.vim](https://github.com/junegunn/fzf.vim)
* [vim-airline/vim-airline](https://github.com/vim-airline/vim-airline)
* [vim-airline/vim-airline-themes](https://github.com/vim-airline/vim-airline)
* [chriskempson/base16-vim](https://github.com/chriskempson/base16-vim)
* [ctrlpvim/ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim)

# Philosophy
I really think vim is a great example of unix philosophy, which indicated the program should do one thing and does it as well as possible, vim is a great example a fantastic editor, but you need to keep in mind that vim is an editor so don't use it as an IDE, I know there are some bundles or YouTube videos that tell you that you can ( and probably can ) make vim a complete IDE and still be faster that all those heavy bloated IDEs out there but I think when it comes to something like vim you need to look differently do the thing vim is a great in, edit your code in it but when it comes for example VCS why not use terminal and the official git client ? why have all these layers of abstraction ? vim-fugitive is an amazing plugin by amazing [tpope](https://github.com/tpope) but it can never reach git cli so why use it when we are in vim and only few keys from git itself.

# Keybindings
All keybindings start with SPC as leader but I tend to keep my keybindings list short, since vim is available almost everywhere and you cannot just carry your $MYVIMRC with you so it's better to stick with defaults and be able to use vim everywhere.

# License 
This configuration uses GPLV3 ( GNU General Public License V3 ).
