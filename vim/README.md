# Vim Configuration
I use neovim so maybe some things are not fully compatible with vim itself, I didn't notice anything myself but jut FYI.

# Plugins
* tpope/vim-vinegar => some improvements over netrw ( vim's default file manager )
* tpope/vim-jdady => json prettify and linting.
* tpope/vim-surround => adds surrounding text objects like parents and quotes.
* tpope/vim-commentary => adds comment support for various languages
* fatih/vim-go => one of best vim plugins I think, full golang support in vim.
* ekalinin/dockerfile.vim => dockerfile syntax highlighting.
* tpope/vim-fugitive => vim wrapper for git
* michaeljsmith/vim-indent-object => vim text object for identifying indented text as a vim text object.
* prabirshrestha/async.vim => async jobs for vim
* prabirshrestha/vim-lsp => language server protocol support for vim ( for autocompletion and linting and many other stuff )
* ryanolsonx/vim-lsp-python => python lsp server support
* pearofducks/ansible-vim => ansible syntax highlighting for vim
* airblade/vim-gitgutter => shows what changed in current file
* terryma/vim-multiple-cursors => multiple cursor support for vim
* junegunn/fzf => fantastic fuzzy search + Rg for grep
* junegunn/fzf.vim => vim bindings for fzf and Rg
* vim-airline/vim-airline => modeline
* vim-airline/vim-airline-themes => modeline themes
# Philosophy
I really think vim is a great example of unix philosophy, which indicated the program should do one thing and does it as well as possible, vim is a great example a fantastic editor, but you need to keep in mind that vim is an editor so don't use it as an IDE, I know there are some bundles or YouTube videos that tell you that you can ( and probably can ) make vim a complete IDE and still be faster that all those heavy bloated IDEs out there but I think when it comes to something like vim you need to look differently do the thing vim is a great in, edit your code in it but when it comes for example VCS why not use terminal and the official git client ? why have all these layers of abstraction ?

# Keybindings
All keybindings start with SPC as leader but I tend to keep my keybindings list short, since vim is available almost everywhere and you cannot just carry your $MYVIMRC with you so it's better to stick with defaults and be able to use vim everywhere.

# License 
This configuration uses GPLV3 ( GNU General Public License V3 ).
